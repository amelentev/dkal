// Copyright (c) Microsoft Corporation
// All rights reserved

namespace DkalLanguage
{
    using System;
    using System.Collections.Generic;
    using System.ComponentModel.Composition;
    using System.Text.RegularExpressions;
    using Microsoft.VisualStudio.Text;
    using Microsoft.VisualStudio.Text.Classification;
    using Microsoft.VisualStudio.Text.Editor;
    using Microsoft.VisualStudio.Text.Tagging;
    using Microsoft.VisualStudio.Utilities;

    [Export(typeof(ITaggerProvider))]
    [ContentType("dkal")]
    [TagType(typeof(DkalTokenTag))]
    internal sealed class DkalTokenTagProvider : ITaggerProvider
    {

        public ITagger<T> CreateTagger<T>(ITextBuffer buffer) where T : ITag
        {
            return new DkalTokenTagger(buffer) as ITagger<T>;
        }
    }

    public class DkalTokenTag : ITag 
    {
        public DkalTokenTypes type { get; private set; }

        public DkalTokenTag(DkalTokenTypes type)
        {
            this.type = type;
        }
    }

    internal sealed class DkalTokenTagger : ITagger<DkalTokenTag>
    {

        ITextBuffer _buffer;
        IDictionary<string, DkalTokenTypes> _dkalTypes;

        internal DkalTokenTagger(ITextBuffer buffer)
        {
            _buffer = buffer;
            _dkalTypes = new Dictionary<string, DkalTokenTypes>();
            _dkalTypes[@"/\*(.|[\n])*?\*/"] = DkalTokenTypes.DkalComment; // XXX not working unless delimiters are in the same line
            _dkalTypes[@"//[^\n]*"] = DkalTokenTypes.DkalComment;

            _dkalTypes[@"(?-i)\b[A-Z]+([A-Z]|[a-z]|_|[0-9])*[.]([A-Z]|[a-z]|_|[0-9])*\b"] = DkalTokenTypes.DkalType; // types

            _dkalTypes[@"(?-i)\bwith\b"] = DkalTokenTypes.DkalKeyword; // with
            _dkalTypes[@"(?-i)\bdo\b"] = DkalTokenTypes.DkalKeyword; // do
            _dkalTypes[@"(?-i)\bupon\b"] = DkalTokenTypes.DkalKeyword; // upon
            _dkalTypes[@"(?-i)\bif\b"] = DkalTokenTypes.DkalKeyword; // if
            _dkalTypes[@"(?-i)\bsend\b"] = DkalTokenTypes.DkalKeyword; // send
            _dkalTypes[@"(?-i)\bsay\b"] = DkalTokenTypes.DkalKeyword; // say
            _dkalTypes[@"(?-i)\bto\b"] = DkalTokenTypes.DkalKeyword; // to
            _dkalTypes[@"(?-i)\blearn\b"] = DkalTokenTypes.DkalKeyword; // learn
            _dkalTypes[@"(?-i)\bforget\b"] = DkalTokenTypes.DkalKeyword; // forget
            _dkalTypes[@"(?-i)\binstall\b"] = DkalTokenTypes.DkalKeyword; // install
            _dkalTypes[@"(?-i)\buninstall\b"] = DkalTokenTypes.DkalKeyword; // uninstall
            _dkalTypes[@"(?-i)\bsaid\b"] = DkalTokenTypes.DkalKeyword; // said
            _dkalTypes[@"(?-i)\basInfon\b"] = DkalTokenTypes.DkalKeyword; // asInfon
            _dkalTypes[@"(?-i)\bfalse\b"] = DkalTokenTypes.DkalKeyword; // false
            _dkalTypes[@"(?-i)\btrue\b"] = DkalTokenTypes.DkalKeyword; // true

            _dkalTypes[@"(?-i)\b[A-Z]+([A-Z]|[a-z]|_|[0-9])*\b"] = DkalTokenTypes.DkalVariable; // variables (identifiers starting with UPPERCASE)

            _dkalTypes[@"\b([-]|[0-9])[0-9]*\b"] = DkalTokenTypes.DkalNumber; // integer numbers
            
            _dkalTypes[@"""[^""\\]*(?:\\.[^""\\]*)*"""] = DkalTokenTypes.DkalString; // string literals

            _dkalTypes[@"(?-i)\btype\b"] = DkalTokenTypes.DkalDirective; // type
            _dkalTypes[@"(?-i)\brelation\b"] = DkalTokenTypes.DkalDirective; // returns
            _dkalTypes[@"(?-i)\bmacro\b"] = DkalTokenTypes.DkalDirective; // function
            _dkalTypes[@"(?-i)\bsubstrate\b"] = DkalTokenTypes.DkalDirective; // substrate
            _dkalTypes[@"(?-i)\bnamespaces\b"] = DkalTokenTypes.DkalDirective; // namespaces
        }

        public event EventHandler<SnapshotSpanEventArgs> TagsChanged
        {
            add { }
            remove { }
        }

        public IEnumerable<ITagSpan<DkalTokenTag>> GetTags(NormalizedSnapshotSpanCollection spans)
        {
            foreach (SnapshotSpan curSpan in spans)
            {
                foreach (string dkalType in _dkalTypes.Keys)
                {
                    Regex dkalRegex = new Regex(dkalType);

                    foreach (Match match in dkalRegex.Matches(curSpan.GetText()))
                    {
                        var tokenSpan = new SnapshotSpan(curSpan.Snapshot, new Span(curSpan.Start.Position + match.Index, match.Length));
                        if (tokenSpan.IntersectsWith(curSpan))
                            yield return new TagSpan<DkalTokenTag>(tokenSpan, new DkalTokenTag(_dkalTypes[dkalType]));
                    }
                }
            }
        }
    }
}
