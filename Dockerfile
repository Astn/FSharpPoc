FROM anaerobic/fsharp-runtime-nuget

RUN mono nuget.exe restore
RUN xbuild /p:BuildWithMono="true" FSharpPoc.sln

CMD []
ENTRYPOINT ["mono", "RandomReader/bin/Debug/RandomReader.exe"]
