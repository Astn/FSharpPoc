#!/bin/bash
 
if [[ $1 != "" ]]
then
        wget -c "$1";
        sudo shutdown -k 0;
else
        echo "Usage: sudo ./downloadAndShutdown <fileToDownload>";
fi

git pull origin

docker build -t random-reader .

docker run --rm random-reader /r 5000 /c 3