

# Create the shinylive app in the dir called site/
shinylive::export(".", "site")


# run the static server to test
httpuv::runStaticServer("site/")


