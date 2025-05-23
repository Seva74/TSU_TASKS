namespace Компилятор
{
    public static class Programm
    {
        public static void Main()
        {
            var code = FileReader.Read("data.txt");

            List<Terminal> terminals;
            if (LexicalAnalyzer.IsLexicalCorrect(code))
            {
                terminals = LexicalAnalyzer.GetTerminals();
            }
            else
            {
                throw new Exception();
            }

            List<RPNSymbol> rpn;
            if (SyntacticalAnalyzer.IsSyntacticalCorrect(terminals))
            {
                rpn = RPNTranslator.ConvertToRPN(terminals);
            }
            else
            {
                throw new Exception();
            }
            int i = 0;
            foreach (var rpnsymvol in rpn)
            {
                Console.Write($"{i++,3} {rpnsymvol.RPNType}");
                if (rpnsymvol is RPNMark)
                {
                    var r = rpnsymvol as RPNMark;
                    Console.Write(" " + r.Position.ToString());
                }
                Console.WriteLine();
            }
            Console.WriteLine("\n\n\n\n");
            RPNInterpreter.ExecuteInstructions(rpn);

        }
    }
}