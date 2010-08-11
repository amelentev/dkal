using System.ComponentModel.Composition;
using System.Windows.Media;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;

namespace DkalLanguage
{
    #region Format definition

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "DkalDirective")]
    [Name("DkalDirective")]
    //this should be visible to the end user
    [UserVisible(false)]
    //set the priority to be after the default classifiers
    [Order(Before = Priority.Default)]
    internal sealed class DkalDirectiveFormat : ClassificationFormatDefinition
    {
        /// <summary>
        /// Defines the visual format for the "ordinary" classification type
        /// </summary>
        public DkalDirectiveFormat()
        {
            this.DisplayName = "DkalDirective"; //human readable version of the name
            this.ForegroundColor = Colors.Blue;
            this.IsBold = true;
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "DkalKeyword")]
    [Name("DkalKeyword")]
    //this should be visible to the end user
    [UserVisible(false)]
    //set the priority to be after the default classifiers
    [Order(Before = Priority.Default)]
    internal sealed class DkalKeywordFormat : ClassificationFormatDefinition
    {
        /// <summary>
        /// Defines the visual format for the "ordinary" classification type
        /// </summary>
        public DkalKeywordFormat()
        {
            this.DisplayName = "DkalKeyword"; //human readable version of the name
            this.ForegroundColor = Colors.Blue;
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "DkalVariable")]
    [Name("DkalVariable")]
    //this should be visible to the end user
    [UserVisible(false)]
    //set the priority to be after the default classifiers
    [Order(Before = Priority.Default)]
    internal sealed class DkalVariableFormat : ClassificationFormatDefinition
    {
        /// <summary>
        /// Defines the visual format for the "ordinary" classification type
        /// </summary>
        public DkalVariableFormat()
        {
            this.DisplayName = "DkalVariable"; //human readable version of the name
            this.ForegroundColor = Colors.Salmon;
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "DkalSubstrate")]
    [Name("DkalSubstrate")]
    //this should be visible to the end user
    [UserVisible(false)]
    //set the priority to be after the default classifiers
    [Order(Before = Priority.Default)]
    internal sealed class DkalSubstrateFormat : ClassificationFormatDefinition
    {
        /// <summary>
        /// Defines the visual format for the "ordinary" classification type
        /// </summary>
        public DkalSubstrateFormat()
        {
            this.DisplayName = "DkalSubstrate"; //human readable version of the name
            this.ForegroundColor = Colors.Magenta;
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "DkalComment")]
    [Name("DkalComment")]
    //this should be visible to the end user
    [UserVisible(false)]
    //set the priority to be after the default classifiers
    [Order(After = Priority.High)]
    internal sealed class DkalCommentFormat : ClassificationFormatDefinition
    {
        /// <summary>
        /// Defines the visual format for the "ordinary" classification type
        /// </summary>
        public DkalCommentFormat()
        {
            this.DisplayName = "DkalComment"; //human readable version of the name
            this.ForegroundColor = Colors.Green;
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "DkalType")]
    [Name("DkalType")]
    //this should be visible to the end user
    [UserVisible(false)]
    //set the priority to be after the default classifiers
    [Order(Before = Priority.Default)]
    internal sealed class DkalTypeFormat : ClassificationFormatDefinition
    {
        /// <summary>
        /// Defines the visual format for the "ordinary" classification type
        /// </summary>
        public DkalTypeFormat()
        {
            this.DisplayName = "DkalType"; //human readable version of the name
            this.ForegroundColor = Colors.Brown;
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "DkalNumber")]
    [Name("DkalNumber")]
    //this should be visible to the end user
    [UserVisible(false)]
    //set the priority to be after the default classifiers
    [Order(Before = Priority.Default)]
    internal sealed class DkalNumberFormat : ClassificationFormatDefinition
    {
        /// <summary>
        /// Defines the visual format for the "ordinary" classification type
        /// </summary>
        public DkalNumberFormat()
        {
            this.DisplayName = "DkalNumber"; //human readable version of the name
            this.ForegroundColor = Colors.BlueViolet;
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "DkalString")]
    [Name("DkalString")]
    //this should be visible to the end user
    [UserVisible(false)]
    //set the priority to be after the default classifiers
    [Order(Before = Priority.Default)]
    internal sealed class DkalStringFormat : ClassificationFormatDefinition
    {
        /// <summary>
        /// Defines the visual format for the "ordinary" classification type
        /// </summary>
        public DkalStringFormat()
        {
            this.DisplayName = "DkalString"; //human readable version of the name
            this.ForegroundColor = Colors.Red;
        }
    }

    #endregion //Format definition
}
