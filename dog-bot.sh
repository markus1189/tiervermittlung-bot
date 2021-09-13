#!/usr/bin/env nix-shell

#!nix-shell -i bash
#!nix-shell shell.nix

SEND_DELAY="10s"

CURL_RETRIES=5
CURL_RETRY_DELAY=10

delay() {
    echo "Sleeping for ${SEND_DELAY}"
    sleep "${SEND_DELAY}"
}

getLinksForYesterday() {
    curl --retry-delay $CURL_RETRY_DELAY --retry $CURL_RETRIES -s 'https://www.tiervermittlung.de/cgi-bin/haustier/db.cgi?db=hunde5&uid=default&ID=&Tierart=Hund&Rasse=&Groesse=&Geschlecht=weiblich&Alter-gt=3&Alter-lt=15.1&Zeitwert=Monate&Titel=&Name=&Staat=&Land=&PLZ=&PLZ-gt=&PLZ-lt=&Ort=&Grund=&Halter=&Notfall=&Chiffre=&keyword=&Date=&referer=&Nachricht=&E1=&E2=&E3=&E4=&E5=&E6=&E7=&E8=&E9=&E10=&mh=100&sb=0&so=descend&ww=&searchinput=&layout=&session=kNWVQkHlAVH5axV0HJs5&Bild=&video_only=&String_Rasse=&view_records=Suchen' | pup --charset iso-8859-1  --color '#Item_Results json{}' | jq -r '(now | todate | fromdate - 86400 | gmtime | strftime("%d.%m.%Y")) as $yesterday | map({date: .children[1].text, link: .children[0].children[0].children[0].href} | select(.date == $yesterday) | .link)[]'
}

postLink() {
    RES="$(curl --retry-delay $CURL_RETRY_DELAY --retry $CURL_RETRIES -s "${1}")"
    LINKS="$(echo "${RES}" | pup '.img_pic_items attr{src}')"
    TEMP="$(mktemp -d)"

    FLAGS=$(
        cd "${TEMP}" || return

        echo "${LINKS}" | parallel -j6 -- wget --quiet
        find . -type f | while read -r i; do echo "-F $(basename "$i")=@$(pwd)/$i"; done | paste -s -d ' '
         )

    VIDEO_URIS="$(echo "${RES}" | pup 'video attr{src}')"

    JSON_ARRAY=$(
        jo -a $(
            echo "${LINKS}" | while read -r i; do
                NAME="$(basename "$i")"
                jo type=photo media="attach://$NAME"
            done
           )
              )

    echo Sending photos
    curl --retry-delay $CURL_RETRY_DELAY --retry $CURL_RETRIES --silent -XPOST \
         --url "https://api.telegram.org/bot${TELEGRAM_BOT_TOKEN}/sendMediaGroup" \
         -F chat_id="${TELEGRAM_CHAT_ID}" \
         -F media="${JSON_ARRAY}" \
         -F disable_notification=true \
         ${FLAGS}
    echo

    delay

    echo "${VIDEO_URIS}" | while read -r video; do
        if [[ -z "${video}" ]]; then
            :
        else
            echo "Sending video"
            curl --retry-delay $CURL_RETRY_DELAY --retry $CURL_RETRIES -s -F disable_notification=true -F video="${video}" "https://api.telegram.org/bot${TELEGRAM_BOT_TOKEN}/sendVideo?chat_id=${TELEGRAM_CHAT_ID}"
            echo
            delay
        fi
    done

    echo "Sending link"
    curl --retry-delay $CURL_RETRY_DELAY --retry $CURL_RETRIES -s "https://api.telegram.org/bot${TELEGRAM_BOT_TOKEN}/sendMessage?chat_id=${TELEGRAM_CHAT_ID}&text=${1}&disable_notification=true"
    echo
    delay

    # Cleanup TEMP
    rm "${TEMP}"/* && rmdir "${TEMP}"

    echo "Done"

}

filterUrl() {
    curl --retry-delay $CURL_RETRY_DELAY --retry $CURL_RETRIES -s "${1}" |
        pup '#Daten_Item' |
        pandoc -f html -t plain |
        grep -q -i \
             -e dackel \
             -e drahthaar \
             -e griffon \
             -e 'englisch setter' \
             -e '\bbrake\b' \
             -e '\bmalteser\b'
}

main() {
    echo "Started at $(date)"
    getLinksForYesterday | while read -r l; do
        if filterUrl "${l}"; then
            echo "Discarding: ${l}"
        else
            echo "Processing: ${l}"
            postLink "${l}"
        fi
    done
}

main
