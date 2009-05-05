using System;
using System.Reflection;
using System.Windows.Forms;
using Microsoft.FSharp.Collections;
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

            List<LispVal> program;
            try
            {
                program = Parser.parseString(text);
            }
            catch (Exception ex)
            {
                MessageBox.Show(this, "Parse error: " + ex.Message);
                return;
            }

            Type programType;
            try
            {
                programType = Compiler.compileToMemory(new AssemblyName("output"), program).Item2;
            }
            catch (Exception ex)
            {
                MessageBox.Show(this, "Compiler error: " + ex.Message);
                return;
            }

            var mainMethod = programType.GetMethod("Main");
            var ret = mainMethod.Invoke(null, new object[] { });
            MessageBox.Show(this, "Output:" + Environment.NewLine + Environment.NewLine + ret);
        }
    }
}