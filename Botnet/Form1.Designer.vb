<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Form1
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Me.lbLoadtime = New System.Windows.Forms.Label()
        Me.lbMemory = New System.Windows.Forms.Label()
        Me.Timer1 = New System.Windows.Forms.Timer(Me.components)
        Me.SuspendLayout()
        '
        'lbLoadtime
        '
        Me.lbLoadtime.AutoSize = True
        Me.lbLoadtime.Font = New System.Drawing.Font("Lucida Console", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lbLoadtime.Location = New System.Drawing.Point(12, 9)
        Me.lbLoadtime.Name = "lbLoadtime"
        Me.lbLoadtime.Size = New System.Drawing.Size(89, 11)
        Me.lbLoadtime.TabIndex = 0
        Me.lbLoadtime.Text = "sdfgsdfgsdfg"
        '
        'lbMemory
        '
        Me.lbMemory.AutoSize = True
        Me.lbMemory.Font = New System.Drawing.Font("Lucida Console", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lbMemory.Location = New System.Drawing.Point(12, 38)
        Me.lbMemory.Name = "lbMemory"
        Me.lbMemory.Size = New System.Drawing.Size(89, 11)
        Me.lbMemory.TabIndex = 1
        Me.lbMemory.Text = "sdfgsdfgsdfg"
        '
        'Timer1
        '
        Me.Timer1.Enabled = True
        Me.Timer1.Interval = 5000
        '
        'Form1
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(284, 264)
        Me.Controls.Add(Me.lbMemory)
        Me.Controls.Add(Me.lbLoadtime)
        Me.Name = "Form1"
        Me.Text = "Server"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents lbLoadtime As System.Windows.Forms.Label
    Friend WithEvents lbMemory As System.Windows.Forms.Label
    Friend WithEvents Timer1 As System.Windows.Forms.Timer

End Class
