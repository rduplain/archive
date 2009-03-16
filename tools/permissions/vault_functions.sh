# Start a new bash shell with this as the rc file.
# bash --rcfile <this-file>

dbbin=/usr/local/pgsql/bin/psql # default if no psql in PATH
dbuser=dave
dbname=vault

old="date '1970-01-01'"

explanations=( )

dbwhich=`which psql`

[[ -x $dbwhich ]] && dbbin=$dbwhich

dbcall="$dbbin -U $dbuser $dbname"

function explain () {
    # Optionally define this function contingent on a verbose flag.
    # Store explanation for vault help
    echo $@
    explanations=( "${explanations[@]}" "$*" )
}

explain
explain "Use the following functions to query the vault database."

function db () {
    if [ $# -eq 0 ]; then
        $dbcall
    else
        $dbcall -tqc "$@"
    fi
}
explain "db [<query>] - call the database with the query, or open a shell."

function vault_search () {
    local nothing=nothing
    for u in `db "select name from users where name ilike '%$@%';"`; do
        echo "user: $u" 2>/dev/null
        unset nothing
    done

    for p in `db "select name from projects where name ilike '%$@%';"`; do
        echo "project: $p" 2>/dev/null
        unset nothing
    done

    if [ -z "$nothing" ]; then
        return 0
    else
        return 1
    fi
}
explain "vault search <project> - list projects with names like <project>."
explain "vault search <username> - list usernames like <username>."

function vault_project () {
    db "select user_name from permissions where project_name = '$@';"
}
explain "vault project <project> - list users with <project> permission."

function vault_projects () {
    db "select name from projects order by name;"
}
explain "vault projects - list all projects."

function vault_user () {
    db "select project_name from permissions where user_name = '$@';"
}
explain "vault user <username> - list projects of user w/<username>."

function vault_users () {
    db "select name from users order by name;"
}
explain "vault users - list all users."

function vault_grant () {
    local user_id
    local project_id

    local user_name=$1
    shift;
    local project_name=$1
    shift;

    user_id=`db "select id from users where name = '$user_name';"`
    if [ -z "$user_id" ]; then
        db "insert into users (name) values ('$user_name');"
        vault_grant $user_name $project_name $@
    else
        if [ -z "$project_name" ]; then
            return 1
        fi
        project_id=`db "select id from projects where name = '$project_name';"`
        if [ -z "$project_id" ]; then 
            echo "$project_name not found"
            return 1
        fi
        local query
        query="insert into projects_users (user_id, project_id) values"
        query="$query ($user_id, $project_id);"
        db "$query"
    fi
}
explain "vault grant <username> - ensure <username> is in vault."
explain "vault grant <username> <project> - grant <project> to <username>."

function vault_revoke () {
    local user_id
    local project_id

    local user_name=$1
    shift;
    local project_name=$1
    shift;

    user_id=`db "select id from users where name = '$user_name';"`
    if [ -z "$user_id" ]; then
        return 0;
    else
        if [ -z "$project_name" ]; then
            db "delete from projects_users where user_id = $user_id;"
            db "delete from users where id = $user_id;"
            return $?
        fi
        project_id=`db "select id from projects where name = '$project_name';"`
        if [ -z "$project_id" ]; then 
            echo "$project_name not found"
            return 1
        fi
        local query
        query="delete from projects_users where user_id = $user_id"
        query="$query and project_id = $project_id;"
        db "$query"
    fi
}
explain "vault revoke <username> - revoke priveleges from <username> & delete."
explain "vault revoke <username> <project> - revoke <project> from <username>."

function vault_lock () {
    # set lastdate to NULL, which defaults to proprietary
    local project_name=$1
    shift;

    db "update projects set lastdate = NULL where name = '$project_name';"
}
explain "vault lock <project> - set <project> to be interpreted proprietary."

function vault_unlock () {
    # set lastdate to long enough ago, how does the epoch sound?
    local project_name=$1
    shift;

    db "update projects set lastdate = $old where name = '$project_name';"
}
explain "vault unlock <project> - set <project> to be interpreted as public."

function vault_age () {
    local project_name=$1
    shift;

    local f=lastdate
    local lastdate=`db "select $f from projects where name = '$project_name';"`
    lastdate=${lastdate// /}
    if [ -n "$lastdate" ]; then
        db "select $f, age($f) from projects where name = '$project_name';"
    else
        echo unknown
        return 1
    fi
}
explain "vault age <project> - display lastdate and age of <project>"

function vault_help () {
    for ((i=0; i<${#explanations[@]}; i++)); do
        echo ${explanations[i]}
    done
}
explain "vault help - show these help messages"

function vault () {
    local vault_command=$1;
    shift;

    vault_${vault_command} $@
}

# Assuming we were launched as: bash --rcfile <this-file> ...
source $HOME/.bashrc 2>/dev/null 1>&2
