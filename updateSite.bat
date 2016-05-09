rm -rf latest/api/*
cp -R /src/shapes/validating/target/scala-2.11/api/* latest/api
git add -A
git commit -m "Updated site"
git push