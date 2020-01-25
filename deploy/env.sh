# Sets all environment variables defined in deploy/env.json.
for k in $(jq -r 'keys[]' deploy/env.json); do
    v=$(jq -r ".$k" deploy/env.json)
    export "$k=$v"
    echo "$k=$v" 1>&2
done
