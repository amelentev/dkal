// Copyright (c) Microsoft Corporation
// All rights reserved

namespace DkalLanguage
{
    using System;
    using System.Collections.Generic;
    using System.ComponentModel.Composition;
    using Microsoft.VisualStudio.Text;
    using Microsoft.VisualStudio.Text.Classification;
    using Microsoft.VisualStudio.Text.Editor;
    using Microsoft.VisualStudio.Text.Tagging;
    using Microsoft.VisualStudio.Utilities;

    [Export(typeof(ITaggerProvider))]
    [ContentType("dkal")]
    [TagType(typeof(ClassificationTag))]
    internal sealed class DkalClassifierProvider : ITaggerProvider
    {

        [Export]
        [Name("dkal")]
        [BaseDefinition("code")]
        internal static ContentTypeDefinition DkalContentType = null;

        [Export]
        [FileExtension(".dkal")]
        [ContentType("dkal")]
        internal static FileExtensionToContentTypeDefinition DkalFileType = null;

        [Import]
        internal IClassificationTypeRegistryService ClassificationTypeRegistry = null;

        [Import]
        internal IBufferTagAggregatorFactoryService aggregatorFactory = null;

        public ITagger<T> CreateTagger<T>(ITextBuffer buffer) where T : ITag
        {

            ITagAggregator<DkalTokenTag> dkalTagAggregator = 
                                            aggregatorFactory.CreateTagAggregator<DkalTokenTag>(buffer);

            return new DkalClassifier(buffer, dkalTagAggregator, ClassificationTypeRegistry) as ITagger<T>;
        }
    }

    internal sealed class DkalClassifier : ITagger<ClassificationTag>
    {
        ITextBuffer _buffer;
        ITagAggregator<DkalTokenTag> _aggregator;
        IDictionary<DkalTokenTypes, IClassificationType> _dkalTypes;

        internal DkalClassifier(ITextBuffer buffer, 
                               ITagAggregator<DkalTokenTag> dkalTagAggregator, 
                               IClassificationTypeRegistryService typeService)
        {
            _buffer = buffer;
            _aggregator = dkalTagAggregator;
            _dkalTypes = new Dictionary<DkalTokenTypes, IClassificationType>();
            _dkalTypes[DkalTokenTypes.DkalDirective] = typeService.GetClassificationType("DkalDirective");
            _dkalTypes[DkalTokenTypes.DkalKeyword] = typeService.GetClassificationType("DkalKeyword");
            _dkalTypes[DkalTokenTypes.DkalComment] = typeService.GetClassificationType("DkalComment");
            _dkalTypes[DkalTokenTypes.DkalNumber] = typeService.GetClassificationType("DkalNumber");
            _dkalTypes[DkalTokenTypes.DkalString] = typeService.GetClassificationType("DkalString");
            _dkalTypes[DkalTokenTypes.DkalSubstrate] = typeService.GetClassificationType("DkalSubstrate");
            _dkalTypes[DkalTokenTypes.DkalVariable] = typeService.GetClassificationType("DkalVariable");
            _dkalTypes[DkalTokenTypes.DkalType] = typeService.GetClassificationType("DkalType");
        }

        public event EventHandler<SnapshotSpanEventArgs> TagsChanged
        {
            add { }
            remove { }
        }

        public IEnumerable<ITagSpan<ClassificationTag>> GetTags(NormalizedSnapshotSpanCollection spans)
        {

            foreach (var tagSpan in this._aggregator.GetTags(spans))
            {
                var tagSpans = tagSpan.Span.GetSpans(spans[0].Snapshot);
                yield return 
                    new TagSpan<ClassificationTag>(tagSpans[0], 
                                                   new ClassificationTag(_dkalTypes[tagSpan.Tag.type]));
            }
        }
    }
}
