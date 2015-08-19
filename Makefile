./bin/main: ./src/main.scala ./bin/syntax ./bin/interpreter ./bin/parser
	scalac src/main.scala -d bin/ -cp ./bin/:./lib/java-cup-11b-runtime.jar

./bin/syntax: ./src/syntax.scala
	@mkdir -p ./bin/
	scalac ./src/syntax.scala -d ./bin/ -cp ./bin/

./bin/interpreter: ./src/interpreter.scala ./bin/syntax
	scalac ./src/interpreter.scala -d ./bin/ -cp ./bin/

./bin/parser: ./bin/syntax ./src/Lexer.java ./src/Parser.java
	javac -sourcepath ./src/ \
    		-cp ./lib/java-cup-11b-runtime.jar:./bin/:$(SCALA_HOME)/lib/scala-library.jar \
    		-d ./bin/ ./src/Lexer.java ./src/Parser.java ./src/sym.java

./src/Lexer.java: ./src/lexer.flex
	java -jar ./lib/jflex-1.6.0.jar -d ./src/ --nobak ./src/lexer.flex

./src/Parser.java: ./src/parser.cup
	java -cp ./lib/ -jar ./lib/java-cup-11b.jar -destdir ./src/ \
            -parser Parser ./src/parser.cup

all: ./bin/main

clean:
	@echo "Cleaning build..."
	@rm ./src/{Lexer.java,Parser.java,sym.java}
	@rm -r ./bin/
	@echo "Done"
