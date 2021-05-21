﻿using System.Reflection;
using JetBrains.ReSharper.Plugins.FSharp.TypeProvidersLoader.Protocol.Cache;
using JetBrains.ReSharper.Plugins.FSharp.TypeProvidersLoader.Protocol.ModelCreators;
using JetBrains.ReSharper.Plugins.FSharp.TypeProvidersProtocol.Utils;
using JetBrains.Rider.FSharp.TypeProvidersProtocol.Client;
using JetBrains.Util;
using Microsoft.FSharp.Core.CompilerServices;
using static FSharp.Compiler.ExtensionTyping;

namespace JetBrains.ReSharper.Plugins.FSharp.TypeProvidersLoader.Protocol
{
  public class TypeProvidersContext
  {
    public ILogger Logger { get; }
    public ITypeProvidersLoader TypeProvidersLoader { get; }
    public TypeProvidersCache TypeProvidersCache { get; }
    public IProvidedCache<(ProvidedConstructorInfo, int), int> ProvidedConstructorsCache { get; }
    public IBiDirectionalProvidedCache<ProvidedAssembly, int> ProvidedAssembliesCache { get; }
    public IProvidedCache<(ProvidedMethodInfo model, int typeProviderId), int> ProvidedMethodsCache { get; }
    public IProvidedCache<(ProvidedPropertyInfo, int), int> ProvidedPropertyCache { get; }

    public IBiDirectionalProvidedCache<ProvidedType, int> ProvidedTypesCache { get; }

    public TypeProviderCreator TypeProviderRdModelsCreator { get; }

    public IProvidedRdModelsCreatorWithCache<ProvidedType, RdOutOfProcessProvidedType, int>
      ProvidedTypeRdModelsCreator { get; }

    public IProvidedRdModelsCreator<IProvidedNamespace, RdProvidedNamespace>
      ProvidedNamespaceRdModelsCreator { get; }

    public IProvidedRdModelsCreator<ProvidedParameterInfo, RdProvidedParameterInfo>
      ProvidedParameterRdModelsCreator { get; }

    public IProvidedRdModelsCreatorWithCache<ProvidedPropertyInfo, RdProvidedPropertyInfo, int>
      ProvidedPropertyRdModelsCreator { get; }

    public IProvidedRdModelsCreatorWithCache<ProvidedMethodInfo, RdProvidedMethodInfo, int>
      ProvidedMethodRdModelsCreator { get; }

    public IProvidedRdModelsCreatorWithCache<ProvidedAssembly, RdProvidedAssembly, int>
      ProvidedAssemblyRdModelsCreator { get; }

    public IProvidedRdModelsCreator<ProvidedFieldInfo, RdProvidedFieldInfo>
      ProvidedFieldRdModelsCreator { get; }

    public IProvidedRdModelsCreator<ProvidedEventInfo, RdProvidedEventInfo> ProvidedEventRdModelsCreator { get; }

    public IProvidedRdModelsCreatorWithCache<ProvidedConstructorInfo, RdProvidedConstructorInfo, int>
      ProvidedConstructorRdModelsCreator { get; }

    public IProvidedRdModelsCreator<CustomAttributeData, RdCustomAttributeData>
      ProvidedCustomAttributeRdModelsCreator { get; }

    public TypeProvidersContext(ILogger logger)
    {
      Logger = logger;
      TypeProvidersLoader = new TypeProvidersLoader();

      TypeProvidersCache = new TypeProvidersCache();
      ProvidedTypesCache = new ProvidedTypesCache(ProvidedTypesComparer.Instance);
      ProvidedPropertyCache = new SimpleProvidedCache<ProvidedPropertyInfo>();
      ProvidedMethodsCache = new SimpleProvidedCache<ProvidedMethodInfo>();
      ProvidedAssembliesCache = new ProvidedAssembliesCache(new ProvidedAssembliesComparer());
      ProvidedConstructorsCache = new SimpleProvidedCache<ProvidedConstructorInfo>();

      TypeProviderRdModelsCreator = new TypeProviderCreator(this);
      ProvidedTypeRdModelsCreator = new ProvidedTypeCreator(this);
      ProvidedNamespaceRdModelsCreator = new ProvidedNamespaceCreator(this);
      ProvidedParameterRdModelsCreator = new ProvidedParameterCreator(this);
      ProvidedPropertyRdModelsCreator = new ProvidedPropertyCreator(this);
      ProvidedMethodRdModelsCreator = new ProvidedMethodCreator(this);
      ProvidedAssemblyRdModelsCreator = new ProvidedAssemblyCreator(this);
      ProvidedFieldRdModelsCreator = new ProvidedFieldCreator(this);
      ProvidedEventRdModelsCreator = new ProvidedEventCreator(this);
      ProvidedConstructorRdModelsCreator = new ProvidedConstructorCreator(this);
      ProvidedCustomAttributeRdModelsCreator = new ProvidedCustomAttributeCreator();
    }
  }
}