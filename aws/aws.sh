#!/bin/bash

set -euo pipefail

root=$PWD

vault="$root/$1"
shift
etc="$root/$1"
shift
configure_py="$root/$1"
shift

. "$etc"

main() {
	config=~/.config/$SITE/aws
	[ -d "$config" ] || make_config_dir

	export AWS_CONFIG_FILE="$config/config"
	export AWS_SHARED_CREDENTIALS_FILE="$config/creds"
	export AWS_DEFAULT_REGION="$1"
	export AWS_REGION="$1"
	export AWS_PROFILE="$2"
	shift 2

	case "$1"x in
	configurex)
		shift
		configure "$@"
		exit $?
		;;
	*)
		vault exec "$@"
		;;
	esac
}

configure() {
	account="$1"
	user="$2"

	cat <<-CONFIG >"$AWS_CONFIG_FILE"
		[profile default]
		region = $AWS_DEFAULT_REGION
		
		[profile $AWS_PROFILE]
		source_profile = default
		mfa_serial=arn:aws:iam::${account}:mfa/${user}
	CONFIG

	vault add default
	vault exec "$configure_py" "$account" "$user"
}

vault() {
	case "$1"x in
	loginx | addx | removex | rotatex) "$vault" --keychain="$SITE" "$@" ;;
	execx)
		shift
		exec "$vault" --keychain="$SITE" exec --assume-role-ttl=60m "$AWS_PROFILE" "$@"
		;;
	esac
}

vault_exec() {
	exec "$vault" \
		--keychain="$SITE" \
		exec \
		--assume-role-ttl=60m "${AWS_PROFILE}" \
		-- "$@"
}

make_config_dir() {
	mkdir -m 777 -p "$config"
}

main "$@"
exit $?
