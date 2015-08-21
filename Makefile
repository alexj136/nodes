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

./bin/parser: ./bin/syntax ./gen/Lexer.java ./gen/Parser.java
	javac -sourcepath ./src/ -cp $(JAVA_CLASSPATH) \
    		-d ./bin/ ./gen/Lexer.java ./gen/Parser.java ./gen/sym.java

./gen/Lexer.java: ./src/lexer.flex
	@mkdir -p ./gen/
	java -jar ./lib/jflex-1.6.0.jar -d ./gen/ --nobak ./src/lexer.flex

./gen/Parser.java: ./src/parser.cup
	@mkdir -p ./gen/
	java -cp ./lib/ -jar ./lib/java-cup-11b.jar -destdir ./gen/ \
            -parser Parser ./src/parser.cup

all: ./bin/main

test: ./bin/test
	@scala -cp $(SCALA_CLASSPATH) test.RunTests

clean:
	@echo "Cleaning build..."
	@rm -r ./bin/ ./gen/
	@echo "Done"
