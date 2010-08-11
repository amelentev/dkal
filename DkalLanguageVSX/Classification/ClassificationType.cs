using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;

namespace DkalLanguage
{
    internal static class OrdinaryClassificationDefinition
    {
        #region Type definition

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("DkalDirective")]
        internal static ClassificationTypeDefinition DkalDirective = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("DkalKeyword")]
        internal static ClassificationTypeDefinition DkalKeyword = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("DkalComment")]
        internal static ClassificationTypeDefinition DkalComment = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("DkalNumber")]
        internal static ClassificationTypeDefinition DkalNumber = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("DkalString")]
        internal static ClassificationTypeDefinition DkalString = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("DkalVariable")]
        internal static ClassificationTypeDefinition DkalVariable = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("DkalSubstrate")]
        internal static ClassificationTypeDefinition DkalSubstrate = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("DkalType")]
        internal static ClassificationTypeDefinition DkalType = null;

        #endregion
    }
}
