module Jekyll
  class LatexTagBlock < Liquid::Block

    @@prologue = %{
\\documentclass[convert={outext=.png}]{standalone}
\\usepackage{mathpartir}
\\begin{document}
\\begin{mathpar}
}
    @@epilogue = %{
\\end{mathpar}
\\end{document}
    }


    def render(context)
      text = super
      File.open('temp.tex', 'w') { |file| file.write(@@prologue + text + @@epilogue) }
      system("pdflatex --shell-escape temp.tex")
      "done"
    end

  end
end

Liquid::Template.register_tag('latex', Jekyll::LatexTagBlock)
