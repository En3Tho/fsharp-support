using System.Linq;
using FSharp.Compiler.SourceCodeServices;
using JetBrains.Annotations;
using JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Daemon.Highlightings;
using JetBrains.ReSharper.Plugins.FSharp.Util;
using static FSharp.Compiler.PrettyNaming;

namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Features.Daemon
{
  // todo: provide F# specific highlightings and use semantic classification from FCS
  public static class FSharpSymbolHighlightingUtil
  {
    [NotNull]
    public static string GetEntityHighlightingAttributeId([NotNull] this FSharpEntity entity)
    {
      if (entity.IsFSharpModule)
        return FSharpHighlightingAttributeIdsModule.Module;

      if (entity.IsNamespace)
        return FSharpHighlightingAttributeIdsModule.Namespace;

      if (entity.IsEnum)
        return FSharpHighlightingAttributeIdsModule.Enum;

      if (entity.IsDelegate)
        return FSharpHighlightingAttributeIdsModule.Delegate;

      if (entity.IsFSharpUnion)
        return FSharpHighlightingAttributeIdsModule.Union;

      if (entity.IsFSharpRecord)
        return FSharpHighlightingAttributeIdsModule.Record;

      if (entity.IsMeasure)
        return FSharpHighlightingAttributeIdsModule.UnitOfMeasure;

      if (entity.IsInterface)
        return FSharpHighlightingAttributeIdsModule.Interface;

      if (entity.IsException())
        return FSharpHighlightingAttributeIdsModule.Exception;

      if (entity.IsClass)
        return FSharpHighlightingAttributeIdsModule.Class;
      
      if (entity.IsValueType || entity.HasMeasureParameter())
        return FSharpHighlightingAttributeIdsModule.Struct;

      if (entity.IsFSharpAbbreviation)
      {
        var abbr = entity.AbbreviatedType;
        if (abbr.IsStructTupleType)
          return FSharpHighlightingAttributeIdsModule.Struct;
        if (abbr.IsTupleType)
          return FSharpHighlightingAttributeIdsModule.Class;
        if (abbr.IsFunctionType)
          return FSharpHighlightingAttributeIdsModule.Delegate;
      }

      return FSharpHighlightingAttributeIdsModule.Class;
    }

    [NotNull]
    public static string GetMfvHighlightingAttributeId([NotNull] this FSharpMemberOrFunctionOrValue mfv)
    {
      if (mfv.IsEvent || mfv.IsEventAddMethod || mfv.IsEventRemoveMethod || mfv.EventForFSharpProperty != null)
        return FSharpHighlightingAttributeIdsModule.Event;

      var declaringEntity = mfv.DeclaringEntity?.Value;
      if (mfv.IsImplicitConstructor || mfv.IsConstructor)
      {
        if (declaringEntity is null)
          return FSharpHighlightingAttributeIdsModule.Class;

        if (declaringEntity.IsDisposable())
        {
          if (declaringEntity.IsClass)
            return FSharpHighlightingAttributeIdsModule.DisposableClass;
          if (declaringEntity.IsValueType)
            return FSharpHighlightingAttributeIdsModule.DisposableStruct;
          if (declaringEntity.IsInterface)
            return FSharpHighlightingAttributeIdsModule.DisposableInterface;
          return FSharpHighlightingAttributeIdsModule.DisposableLocalValue; // fallback?
        }

        if (declaringEntity.IsException())
          return FSharpHighlightingAttributeIdsModule.Exception;

        return declaringEntity.IsValueType
          ? FSharpHighlightingAttributeIdsModule.Struct
          : FSharpHighlightingAttributeIdsModule.Class;
      }

      if (declaringEntity is { })
      {
        if (mfv.IsModuleValueOrMember && (!declaringEntity.IsFSharpModule || mfv.IsExtensionMember))
          if (mfv.IsProperty || mfv.IsPropertyGetterMethod || mfv.IsPropertySetterMethod)
            return mfv.IsExtensionMember
              ? FSharpHighlightingAttributeIdsModule.ExtensionProperty
              : mfv.HasSetterMethod || declaringEntity.ContainsMatchingSetterProperty(mfv)
                ? FSharpHighlightingAttributeIdsModule.MutableProperty
                : FSharpHighlightingAttributeIdsModule.Property;
          else
            return mfv.IsExtensionMember
              ? FSharpHighlightingAttributeIdsModule.ExtensionMethod
              : FSharpHighlightingAttributeIdsModule.Method;
      }

      if (mfv.IsLiteral())
        return FSharpHighlightingAttributeIdsModule.Literal;

      if (mfv.IsActivePattern)
        return FSharpHighlightingAttributeIdsModule.ActivePatternCase;

      var fsType = mfv.FullType;

      if (IsMangledOpName(mfv.LogicalName) && fsType.IsFunctionType)
      {
        return !mfv.InlineAnnotation.Equals(FSharpInlineAnnotation.NeverInline)
            && !mfv.InlineAnnotation.Equals(FSharpInlineAnnotation.OptionalInline)
          ? FSharpHighlightingAttributeIdsModule.InlineOperator
          : FSharpHighlightingAttributeIdsModule.Operator;
      }

      if (fsType.IsFunctionType || mfv.IsTypeFunction || fsType.IsAbbreviation && fsType.AbbreviatedType.IsFunctionType)
      {
        if (!mfv.InlineAnnotation.Equals(FSharpInlineAnnotation.NeverInline)
         && !mfv.InlineAnnotation.Equals(FSharpInlineAnnotation.OptionalInline))
          return FSharpHighlightingAttributeIdsModule.InlineFunction;
        if (mfv.IsValCompiledAsMethod())
          return FSharpHighlightingAttributeIdsModule.Method;
        return mfv.IsMutable
          ? FSharpHighlightingAttributeIdsModule.MutableFunction
          : FSharpHighlightingAttributeIdsModule.Function;
      }

      if (fsType.IsDisposable() && !mfv.IsModuleValueOrMember && !mfv.IsMemberThisValue && !mfv.IsConstructorThisValue)
        return FSharpHighlightingAttributeIdsModule.DisposableLocalValue;

      if (mfv.IsMutable || mfv.IsRefCell())
        return FSharpHighlightingAttributeIdsModule.MutableValue;

      if (fsType.HasTypeDefinition && fsType.TypeDefinition is var mfvTypeEntity && mfvTypeEntity.IsByRef)
        return FSharpHighlightingAttributeIdsModule.MutableValue;

      if (mfv.IsModuleValueOrMember)
        return FSharpHighlightingAttributeIdsModule.StaticValue;

      if (mfv.IsTopLevelParameter)
        return FSharpHighlightingAttributeIdsModule.Parameter;

      return mfv.IsNestedScopeParameter
        ? FSharpHighlightingAttributeIdsModule.NestedScopeParameter
        : FSharpHighlightingAttributeIdsModule.Value;
    }

    [NotNull]
    public static string GetHighlightingAttributeId([NotNull] this FSharpSymbol symbol)
    {
      switch (symbol)
      {
        case FSharpEntity { IsUnresolved: false } entity:
          return GetEntityHighlightingAttributeId(entity.GetAbbreviatedEntity());

        case FSharpMemberOrFunctionOrValue { IsUnresolved: false } mfv:
          return GetMfvHighlightingAttributeId(mfv.AccessorProperty?.Value ?? mfv);

        case FSharpField field:
          var declaringEntity = field.DeclaringEntity?.Value;
          if (declaringEntity is null
           || declaringEntity.IsFSharpRecord
           || declaringEntity.IsException())
          {
            return field.IsMutable
              ? FSharpHighlightingAttributeIdsModule.MutableProperty
              : FSharpHighlightingAttributeIdsModule.Property;
          }
            
          return field.IsLiteral
            ? FSharpHighlightingAttributeIdsModule.Literal
            : field.IsMutable
              ? FSharpHighlightingAttributeIdsModule.MutableField
              : FSharpHighlightingAttributeIdsModule.Field;

        case FSharpUnionCase _:
          return FSharpHighlightingAttributeIdsModule.UnionCase;

        case FSharpGenericParameter _:
          return FSharpHighlightingAttributeIdsModule.TypeParameter;

        case FSharpActivePatternCase _:
          return FSharpHighlightingAttributeIdsModule.ActivePatternCase;

        case FSharpParameter prm:
          if (prm.IsPropertyConstraint()) return FSharpHighlightingAttributeIdsModule.Property;
          if (prm.IsOperatorConstraint()) return FSharpHighlightingAttributeIdsModule.Operator;
          if (prm.IsMethodConstraint()) return FSharpHighlightingAttributeIdsModule.Method;
          return FSharpHighlightingAttributeIdsModule.Value;
      }

      // some highlighting is needed for tooltip provider
      return FSharpHighlightingAttributeIdsModule.Value;
    }
  }
}
