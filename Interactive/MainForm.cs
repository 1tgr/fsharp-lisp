using System;
using System.Linq;
using System.Windows.Forms;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;
using Tim.Lisp.Core;

namespace Tim.Lisp.Interactive
{
    public partial class MainForm : Form
    {
        public MainForm()
        {
            InitializeComponent();
        }

        private void parseButton_Click(object sender, EventArgs e)
        {
            sexpTextBox.Text = string.Empty;
            primitivesTextBox.Text = string.Empty;

            try
            {
                UpdateTextBoxes(Parse());
            }
            catch (Exception ex)
            {
                MessageBox.Show(this, ex.Message, Text, MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void runButton_Click(object sender, EventArgs e)
        {
            sexpTextBox.Text = string.Empty;
            primitivesTextBox.Text = string.Empty;

            try
            {
                FSharpList<LispVal> code = Parse();
                UpdateTextBoxes(code);

                Action action = (Action) Compiler.compileToDelegate(typeof(Action), code);
                action();
            }
            catch (Exception ex)
            {
                MessageBox.Show(this, ex.Message, Text, MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private FSharpList<LispVal> Parse()
        {
            string text = textBox.SelectedText;
            if (text.Length == 0)
                text = textBox.Text;

            return Parser.parseString(text);
        }

        private void UpdateTextBoxes(FSharpList<LispVal> code)
        {
            sexpTextBox.Text = AnyToString(code);

            FSharpList<LispVal> codeWithPrimitives =
                ListModule.of_seq(
                    SeqModule
                        .of_list(code)
                        .Select(v => CodeGenerator.insertPrimitives(v)));

            primitivesTextBox.Text = AnyToString(codeWithPrimitives);
        }

        private static string AnyToString<T>(T codeWithPrimitives)
        {
            return ExtraTopLevelOperators
                .any_to_string(codeWithPrimitives)
                .Replace("\n", "\r\n");
        }
    }
}