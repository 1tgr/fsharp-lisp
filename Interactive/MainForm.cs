using System;
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

        private void toolStripButton1_Click(object sender, EventArgs e)
        {
            string text = textBox.SelectedText;
            if (text.Length == 0)
                text = textBox.Text;

            FSharpList<LispVal> code = Parser.parseString(text);
            expressionTreeTextBox.Text = ExtraTopLevelOperators.any_to_string(code).Replace("\n", "\r\n");

            Action action = (Action) Compiler.compileToDelegate(typeof(Action), code);
            action();
        }
    }
}