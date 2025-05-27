#!/bin/bash

# Waybar VPN Module for Mullvad VPN
# Adapted from original Polybar script by Shervin S.
# https://github.com/properlypurple/waybar-vpn-controller/blob/master/vpn_module.sh

VPN_CONNECT="mullvad connect"
VPN_DISCONNECT="mullvad disconnect"
VPN_GET_STATUS="mullvad status"
VPN_RELAY_SET_LOCATION="mullvad relay set location"

VPN_LOCATIONS=("se mma" "se got" "se sto" "gb glw" "gb lon" "gb mnc")
CONNECTED=Connected
CONNECTING=Connecting

# COUNTRY_CODES=("al" "au" "at" "be" "br" "bg" "ca" "cz" "dk" "fi" "fr" "de" "gr" "hk" "hu" "ie" "il" "it" "jp" "lv" "lu" "md" "nl" "nz" "no" "pl" "ro" "rs" "sg" "es" "se" "ch" "gb" "ae" "us")
# COUNTRIES=("Albania (al)" "Australia (au)" "Austria (at)" "Belgium (be)" "Brazil (br)" "Bulgaria (bg)" "Canada (ca)" "Czech Republic (cz)" "Denmark (dk)" "Finland (fi)" "France (fr)" "Germany (de)" "Greece (gr)" "Hong Kong (hk)" "Hungary (hu)" "Ireland (ie)" "Israel (il)" "Italy (it)" "Japan (jp)" "Latvia (lv)" "Luxembourg (lu)" "Moldova (md)" "Netherlands (nl)" "New Zealand (nz)" "Norway (no)" "Poland (pl)" "Romania (ro)" "Serbia (rs)" "Singapore (sg)" "Spain (es)" "Sweden (se)" "Switzerland (ch)" "UK (gb)" "United Arab Emirates (ae)" "USA (us)")
# VPN_CODES=("${VPN_LOCATIONS[@]}" "${COUNTRY_CODES[@]}")
# VPN_LOCATIONS+=("${COUNTRIES[@]}")

vpn_status_raw="$($VPN_GET_STATUS 2>/dev/null)"
# VPN_STATUS=$(echo "$vpn_status_raw" | awk '{print $3}')
VPN_STATUS=$(mullvad status | awk 'NR==1 {print $1}')
VPN_LOCATION=$(mullvad status | awk 'NR==4 {print $3,$4}')

vpn_report() {
    if [ "$VPN_STATUS" = "$CONNECTED" ]; then
        #ip_address=$(echo "$vpn_status_raw" | grep -oE '[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+')
        echo "{\"text\": \"$VPN_LOCATION\", \"class\": \"connected\"}"
    elif [ "$VPN_STATUS" = "$CONNECTING" ]; then
        echo '{"text": "Connecting...", "class": "connecting"}'
    else
        echo '{"text": "No VPN", "class": "disconnected"}'
    fi
}

vpn_toggle_connection() {
    if [ "$VPN_STATUS" = "$CONNECTED" ]; then
        $VPN_DISCONNECT
    else
        $VPN_CONNECT
    fi
}

vpn_location_menu() {
    if ! command -v wofi &> /dev/null; then
        echo "wofi not found" >&2
        return
    fi

    OPTIONS=("Connect/Disconnect" "${VPN_LOCATIONS[@]}")
    MENU=$(printf "%s\n" "${OPTIONS[@]}" | wofi --dmenu --prompt "Mullvad VPN")
    MENU=$(echo "$MENU" | xargs)  # trim whitespace

    if [[ "$MENU" == "Connect/Disconnect" ]]; then
        vpn_toggle_connection
        return
    fi

    for i in "${!VPN_LOCATIONS[@]}"; do
        if [[ "$MENU" == "${VPN_LOCATIONS[$i]}" ]]; then
            read -r country city <<< "${VPN_LOCATIONS[$i]}"
            $VPN_RELAY_SET_LOCATION "$country" "$city"
            $VPN_CONNECT
            return
        fi
    done
}

ip_address_to_clipboard() {
    ip_address=$(curl -s https://ipaddr.pub)
    echo "$ip_address" | xclip -selection clipboard
    echo "{\"text\": \"$ip_address\", \"class\": \"copied\"}"
}

case "$1" in
    --toggle-connection) vpn_toggle_connection ;;
    --location-menu) vpn_location_menu ;;
    --ip-address) ip_address_to_clipboard ;;
    *) vpn_report ;;
esac
