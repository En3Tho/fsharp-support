﻿using System.Linq;
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
      if (entity.IsNamespace)
        return FSharpHighlightingAttributeIdsModule.Namespace;

      if (entity.IsEnum)
        return FSharpHighlightingAttributeIdsModule.Enum;

      if (entity.IsDelegate)
        return FSharpHighlightingAttributeIdsModule.Delegate;

      if (entity.IsFSharpModule)
        return FSharpHighlightingAttributeIdsModule.Module;

      if (entity.IsFSharpUnion)
        return FSharpHighlightingAttributeIdsModule.Union;

      if (entity.IsFSharpRecord)
        return FSharpHighlightingAttributeIdsModule.Record;

      if (entity.IsMeasure)
        return FSharpHighlightingAttributeIdsModule.UnitOfMeasure;

      if (entity.IsInterface)
        return FSharpHighlightingAttributeIdsModule.Interface;

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
          return FSharpHighlightingAttributeIdsModule.DisposableValue;

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
        return FSharpHighlightingAttributeIdsModule.Operator;
      
      if (fsType.IsFunctionType || mfv.IsTypeFunction || fsType.IsAbbreviation && fsType.AbbreviatedType.IsFunctionType)
        return mfv.IsMutable
          ? FSharpHighlightingAttributeIdsModule.MutableFunction
          : FSharpHighlightingAttributeIdsModule.Function;

      if (fsType.IsDisposable())
        return FSharpHighlightingAttributeIdsModule.DisposableValue;

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
          if (field.DeclaringEntity?.Value.IsFSharpRecord ?? true)
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
      }

      // some highlighting is needed for tooltip provider
      return FSharpHighlightingAttributeIdsModule.Value;
    }
  }
}
