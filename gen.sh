java -jar ./lib/jflex-1.6.0.jar -d ./src/main/java --nobak lexer.flex
echo "lol"
java -cp ./lib/ -jar ./lib/java-cup-11b.jar -destdir ./src/main/java \
        -parser Parser parser.cup
