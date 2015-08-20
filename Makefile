CUP_RUNTIME=./lib/java-cup-11b-runtime.jar
BIN_DIR=./bin/
SCALA_LIB=$(SCALA_HOME)/lib/scala-library.jar
SCALA_CLASSPATH=$(CUP_RUNTIME):$(BIN_DIR)
JAVA_CLASSPATH=$(SCALA_LIB):$(CUP_RUNTIME):$(BIN_DIR)

./bin/main: ./src/main.scala ./bin/syntax ./bin/interpreter ./bin/parser
	scalac src/main.scala -d bin/ -cp $(SCALA_CLASSPATH)

./bin/test: ./src/test.scala ./bin/main
	scalac src/test.scala -d bin/ -cp $(SCALA_CLASSPATH)

./bin/syntax: ./src/syntax.scala
	@mkdir -p ./bin/
	scalac ./src/syntax.scala -d ./bin/ -cp ./bin/

./bin/interpreter: ./src/interpreter.scala ./bin/syntax
	scalac ./src/interpreter.scala -d ./bin/ -cp ./bin/

./bin/parser: ./bin/syntax ./src/Lexer.java ./src/Parser.java
	javac -sourcepath ./src/ -cp $(JAVA_CLASSPATH) \
    		-d ./bin/ ./src/Lexer.java ./src/Parser.java ./src/sym.java

./src/Lexer.java: ./src/lexer.flex
	java -jar ./lib/jflex-1.6.0.jar -d ./src/ --nobak ./src/lexer.flex

./src/Parser.java: ./src/parser.cup
	java -cp ./lib/ -jar ./lib/java-cup-11b.jar -destdir ./src/ \
            -parser Parser ./src/parser.cup

all: ./bin/main

test: ./bin/test
	@scala -cp $(SCALA_CLASSPATH) test.RunTests

clean:
	@echo "Cleaning build..."
	@rm ./src/{Lexer.java,Parser.java,sym.java}
	@rm -r ./bin/
	@echo "Done"
