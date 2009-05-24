using System;
using System.CodeDom.Compiler;
using System.Reflection;
using System.Windows.Forms;
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

            CompilerResults results = new LispCodeProvider().CompileAssemblyFromSource(new CompilerParameters { GenerateInMemory = true }, text);
            Type programType = results.CompiledAssembly.GetType("Program");
            MethodInfo mainMethod = programType.GetMethod("Main");
            mainMethod.Invoke(null, new object[] { });
        }
    }
}