/**
 * SyntaxHighlighter
 * http://alexgorbatchev.com/
 *
 * @version
 * 2.0.278 (February 03 2009)
 *
 * @author
 * Alex Gorbatchev
 * 
 * @copyright
 * Copyright (C) 2004-2009 Alex Gorbatchev.
 *
 * Licensed under a GNU Lesser General Public License.
 * http://creativecommons.org/licenses/LGPL/2.1/
 *
 * SyntaxHighlighter is donationware. You are allowed to download, modify and distribute 
 * the source code in accordance with LGPL 2.1 license, however if you want to use 
 * SyntaxHighlighter on your site or include it in your product, you must donate.
 * http://alexgorbatchev.com/wiki/SyntaxHighlighter:Donate
 */
SyntaxHighlighter.brushes.Python = function()
{
	// Contributed by Gheorghe Milas
	
    var keywords =  'and assert break class continue def del elif else ' +
                    'except exec finally for from global if import in is ' +
                    'lambda not or pass print raise return try yield while';

    var special =  'None True False self cls class_';

    this.regexList = [
        { regex: SyntaxHighlighter.regexLib.singleLinePerlComments, css: 'comments' },
        { regex: /^\s*@\w+/gm, 										css: 'decorator' },
        { regex: /(['\"]{3})([^\1])*?\1/gm, 						css: 'comments' },
        { regex: /"(?!")(?:\.|\\\"|[^\""\n])*"/gm, 					css: 'string' },
        { regex: /'(?!')(?:\.|(\\\')|[^\''\n])*'/gm, 				css: 'string' },
        { regex: /\+|\-|\*|\/|\%|=|==/gm, 								css: 'keyword' },
        { regex: /\b\d+\.?\w*/g, 									css: 'value' },
        { regex: new RegExp(this.getKeywords(keywords), 'gm'), 		css: 'keyword' },
        { regex: new RegExp(this.getKeywords(special), 'gm'), 		css: 'color1' }
        ];

	this.forHtmlScript(SyntaxHighlighter.regexLib.aspScriptTags);
};

SyntaxHighlighter.brushes.Python.prototype  = new SyntaxHighlighter.Highlighter();
SyntaxHighlighter.brushes.Python.aliases    = ['py', 'python'];
