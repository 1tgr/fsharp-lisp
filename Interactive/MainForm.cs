using System;
using System.Linq;
using System.Windows.Forms;
using FParsec;
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
                FSharpList<Syntax.Expr> code = Parse();
                UpdateTextBoxes(code);

                Action action = (Action) Compiler.compileToDelegate(typeof(Action), code);
                action();
            }
            catch (Exception ex)
            {
                MessageBox.Show(this, ex.Message, Text, MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private FSharpList<Syntax.Expr> Parse()
        {
            string text = textBox.SelectedText;
            if (text.Length == 0)
                text = textBox.Text;

            return Parser.parseString(text);
        }

        private void UpdateTextBoxes(FSharpList<Syntax.Expr> code)
        {
            sexpTextBox.Text = AnyToString(code);
        }

        private static string AnyToString<T>(T codeWithPrimitives)
        {
            return Operators
                .ToString(codeWithPrimitives)
                .Replace("\n", "\r\n");
        }
    }
}