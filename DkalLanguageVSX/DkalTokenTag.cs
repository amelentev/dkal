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

            _dkalTypes[@"(?i)\bprincipal\b"] = DkalTokenTypes.DkalType; // principal
            _dkalTypes[@"(?i)\bint\b"] = DkalTokenTypes.DkalType; // int
            _dkalTypes[@"(?i)\bbool\b"] = DkalTokenTypes.DkalType; // bool
            _dkalTypes[@"(?i)\btext\b"] = DkalTokenTypes.DkalType; // text

            _dkalTypes[@"(?i)\btype\b"] = DkalTokenTypes.DkalKeyword; // type
            _dkalTypes[@"(?i)\bfunction\b"] = DkalTokenTypes.DkalKeyword; // function
            _dkalTypes[@"(?i)\breturns\b"] = DkalTokenTypes.DkalKeyword; // returns
            _dkalTypes[@"(?i)\bis\b"] = DkalTokenTypes.DkalKeyword; // is
            _dkalTypes[@"(?i)\battribute\b"] = DkalTokenTypes.DkalKeyword; // attribute
            _dkalTypes[@"(?i)\bif\b"] = DkalTokenTypes.DkalKeyword; // if
            _dkalTypes[@"(?i)\bknows\b"] = DkalTokenTypes.DkalKeyword; // knows
            _dkalTypes[@"(?i)\bthen\b"] = DkalTokenTypes.DkalKeyword; // then
            _dkalTypes[@"(?i)\bsaid\b"] = DkalTokenTypes.DkalKeyword; // said
            _dkalTypes[@"(?i)\bimplied\b"] = DkalTokenTypes.DkalKeyword; // implied
            _dkalTypes[@"(?i)\btdonS\b"] = DkalTokenTypes.DkalKeyword; // tdonS
            _dkalTypes[@"(?i)\btdonI\b"] = DkalTokenTypes.DkalKeyword; // tdonI
            _dkalTypes[@"(?i)\bsend\s+(with\s+justification\s+)?to\b"] = DkalTokenTypes.DkalKeyword; // send (with justification) to
            _dkalTypes[@"(?i)\bsay\s+(with\s+justification\s+)?to\b"] = DkalTokenTypes.DkalKeyword; // say (with justification) to

            _dkalTypes[@"(?i)\basInfon\b"] = DkalTokenTypes.DkalSubstrate; // asInfon

            _dkalTypes[@"(?-i)\b[A-Z]+([A-Z]|[a-z]|_|[0-9])*\b"] = DkalTokenTypes.DkalVariable; // variables (identifiers starting with UPPERCASE)

            _dkalTypes[@"\b([-]|[0-9])[0-9]*\b"] = DkalTokenTypes.DkalNumber; // integer numbers
            
            _dkalTypes[@"""[^""\\]*(?:\\.[^""\\]*)*"""] = DkalTokenTypes.DkalString; // string literals

            _dkalTypes[@"(?i)#include\b"] = DkalTokenTypes.DkalDirective; // #include
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
