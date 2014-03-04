#!/bin/bash -X

if [ -z "${1}" ]; then
    echo "no branch selected, using current branch"
    _branch=`git branch --no-color | grep "* " | sed s/"* "//`
else
    _branch="$1"
fi

echo "current branch: ${_branch}"
echo "update cameoServer"
git checkout ${_branch}
git pull

echo "update cameoJSClient"
if [ ! -d "./public" ];then
    git clone git@github.com:memoConnect/cameoJSClient.git public
fi

cd public
git checkout ${_branch}
git pull
cd ..
