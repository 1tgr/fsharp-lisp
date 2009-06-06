namespace Tim.Lisp.Interactive
{
    partial class MainForm
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.Windows.Forms.ToolStrip toolStrip1;
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(MainForm));
            System.Windows.Forms.ToolStripButton runButton;
            System.Windows.Forms.SplitContainer splitContainer1;
            System.Windows.Forms.TabControl tabControl1;
            System.Windows.Forms.TabPage tabPage1;
            System.Windows.Forms.TabPage tabPage2;
            this.parseButton = new System.Windows.Forms.ToolStripButton();
            this.textBox = new System.Windows.Forms.TextBox();
            this.sexpTextBox = new System.Windows.Forms.TextBox();
            this.primitivesTextBox = new System.Windows.Forms.TextBox();
            toolStrip1 = new System.Windows.Forms.ToolStrip();
            runButton = new System.Windows.Forms.ToolStripButton();
            splitContainer1 = new System.Windows.Forms.SplitContainer();
            tabControl1 = new System.Windows.Forms.TabControl();
            tabPage1 = new System.Windows.Forms.TabPage();
            tabPage2 = new System.Windows.Forms.TabPage();
            toolStrip1.SuspendLayout();
            splitContainer1.Panel1.SuspendLayout();
            splitContainer1.Panel2.SuspendLayout();
            splitContainer1.SuspendLayout();
            tabControl1.SuspendLayout();
            tabPage1.SuspendLayout();
            tabPage2.SuspendLayout();
            this.SuspendLayout();
            // 
            // toolStrip1
            // 
            toolStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.parseButton,
            runButton});
            toolStrip1.Location = new System.Drawing.Point(0, 0);
            toolStrip1.Name = "toolStrip1";
            toolStrip1.Size = new System.Drawing.Size(292, 25);
            toolStrip1.TabIndex = 0;
            toolStrip1.Text = "toolStrip1";
            // 
            // parseButton
            // 
            this.parseButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.parseButton.Image = ((System.Drawing.Image)(resources.GetObject("parseButton.Image")));
            this.parseButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.parseButton.Name = "parseButton";
            this.parseButton.Size = new System.Drawing.Size(38, 22);
            this.parseButton.Text = "&Parse";
            this.parseButton.Click += new System.EventHandler(this.parseButton_Click);
            // 
            // runButton
            // 
            runButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            runButton.Image = ((System.Drawing.Image)(resources.GetObject("runButton.Image")));
            runButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            runButton.Name = "runButton";
            runButton.Size = new System.Drawing.Size(30, 22);
            runButton.Text = "&Run";
            runButton.Click += new System.EventHandler(this.runButton_Click);
            // 
            // splitContainer1
            // 
            splitContainer1.Dock = System.Windows.Forms.DockStyle.Fill;
            splitContainer1.FixedPanel = System.Windows.Forms.FixedPanel.Panel2;
            splitContainer1.Location = new System.Drawing.Point(0, 25);
            splitContainer1.Name = "splitContainer1";
            splitContainer1.Orientation = System.Windows.Forms.Orientation.Horizontal;
            // 
            // splitContainer1.Panel1
            // 
            splitContainer1.Panel1.Controls.Add(this.textBox);
            // 
            // splitContainer1.Panel2
            // 
            splitContainer1.Panel2.Controls.Add(tabControl1);
            splitContainer1.Size = new System.Drawing.Size(292, 241);
            splitContainer1.SplitterDistance = 25;
            splitContainer1.TabIndex = 3;
            // 
            // textBox
            // 
            this.textBox.AcceptsReturn = true;
            this.textBox.AcceptsTab = true;
            this.textBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.textBox.Location = new System.Drawing.Point(0, 0);
            this.textBox.Multiline = true;
            this.textBox.Name = "textBox";
            this.textBox.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.textBox.Size = new System.Drawing.Size(292, 25);
            this.textBox.TabIndex = 1;
            this.textBox.Text = "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))\r\n(MessageBox.Show (String.F" +
                "ormat \"6! = {0}\" (fact 6)) \"Lisp Demo\")";
            this.textBox.WordWrap = false;
            // 
            // tabControl1
            // 
            tabControl1.Controls.Add(tabPage1);
            tabControl1.Controls.Add(tabPage2);
            tabControl1.Dock = System.Windows.Forms.DockStyle.Fill;
            tabControl1.Location = new System.Drawing.Point(0, 0);
            tabControl1.Name = "tabControl1";
            tabControl1.SelectedIndex = 0;
            tabControl1.Size = new System.Drawing.Size(292, 212);
            tabControl1.TabIndex = 3;
            // 
            // tabPage1
            // 
            tabPage1.Controls.Add(this.sexpTextBox);
            tabPage1.Location = new System.Drawing.Point(4, 22);
            tabPage1.Name = "tabPage1";
            tabPage1.Padding = new System.Windows.Forms.Padding(3);
            tabPage1.Size = new System.Drawing.Size(284, 186);
            tabPage1.TabIndex = 0;
            tabPage1.Text = "S-Expressions";
            tabPage1.UseVisualStyleBackColor = true;
            // 
            // sexpTextBox
            // 
            this.sexpTextBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.sexpTextBox.Location = new System.Drawing.Point(3, 3);
            this.sexpTextBox.Multiline = true;
            this.sexpTextBox.Name = "sexpTextBox";
            this.sexpTextBox.ReadOnly = true;
            this.sexpTextBox.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.sexpTextBox.Size = new System.Drawing.Size(278, 180);
            this.sexpTextBox.TabIndex = 2;
            this.sexpTextBox.WordWrap = false;
            // 
            // tabPage2
            // 
            tabPage2.Controls.Add(this.primitivesTextBox);
            tabPage2.Location = new System.Drawing.Point(4, 22);
            tabPage2.Name = "tabPage2";
            tabPage2.Padding = new System.Windows.Forms.Padding(3);
            tabPage2.Size = new System.Drawing.Size(284, 100);
            tabPage2.TabIndex = 1;
            tabPage2.Text = "Primitives";
            tabPage2.UseVisualStyleBackColor = true;
            // 
            // primitivesTextBox
            // 
            this.primitivesTextBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.primitivesTextBox.Location = new System.Drawing.Point(3, 3);
            this.primitivesTextBox.Multiline = true;
            this.primitivesTextBox.Name = "primitivesTextBox";
            this.primitivesTextBox.ReadOnly = true;
            this.primitivesTextBox.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.primitivesTextBox.Size = new System.Drawing.Size(278, 94);
            this.primitivesTextBox.TabIndex = 3;
            this.primitivesTextBox.WordWrap = false;
            // 
            // MainForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(292, 266);
            this.Controls.Add(splitContainer1);
            this.Controls.Add(toolStrip1);
            this.Name = "MainForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.WindowsDefaultBounds;
            this.Text = "Lisp Compiler Demo";
            toolStrip1.ResumeLayout(false);
            toolStrip1.PerformLayout();
            splitContainer1.Panel1.ResumeLayout(false);
            splitContainer1.Panel1.PerformLayout();
            splitContainer1.Panel2.ResumeLayout(false);
            splitContainer1.ResumeLayout(false);
            tabControl1.ResumeLayout(false);
            tabPage1.ResumeLayout(false);
            tabPage1.PerformLayout();
            tabPage2.ResumeLayout(false);
            tabPage2.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.TextBox textBox;
        private System.Windows.Forms.TextBox sexpTextBox;
        private System.Windows.Forms.TextBox primitivesTextBox;
        private System.Windows.Forms.ToolStripButton parseButton;
    }
}

