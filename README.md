# ifc-tools-clj

A Clojure library for [IFC](https://technical.buildingsmart.org/standards/ifc/).
- IFC structures as clojure maps.
- Serializing/deserializing STEP files (XML on the way).
- Malli schemas for IFC entities and types 
- In early prototyping phase.

## Usage
```clojure
(require '[ifc-tools-clj.step.core :as step])
```
### Deserialization 
Deafult is IFC STEP file is deserialized into nested map:
```clojure
(clojure.pprint/pprint
  (step/ifc-deserialize 
    (slurp "test/ifc_tools_clj/samples/horizontal-alignment.ifc")))
```
```clojure
({:IfcProject
  {:RepresentationContexts
   [{:IfcGeometricRepresentationContext
     {:ContextType "Model",
      :CoordinateSpaceDimension 3,
      :Precision 1.0E-5,
      :WorldCoordinateSystem
      {:IfcAxis2Placement3D
       {:Location {:IfcCartesianPoint {:Coordinates (0.0 0.0 0.0)}},
        :Axis {:IfcDirection {:DirectionRatios (0.0 0.0 1.0)}},
        :RefDirection
        {:IfcDirection {:DirectionRatios (1.0 0.0 0.0)}}}},
      :TrueNorth {:IfcDirection {:DirectionRatios (0.0 1.0 0.0)}}}}],
   :GlobalId "2OKPkVpxT608bfa7KyQSyR",
   :Name "Test Project",
   :Description "A test project for IFC-dotnet.",
   :UnitsInContext
   {:IfcUnitAssignment
    {:Units
     [{:IfcSIUnit
       {:Dimensions "", :UnitType :LENGTHUNIT, :Name :METRE}}
      {:IfcSIUnit
       {:Dimensions "", :UnitType :AREAUNIT, :Name :SQUARE_METRE}}
      {:IfcSIUnit
       {:Dimensions "", :UnitType :VOLUMEUNIT, :Name :CUBIC_METRE}}
      {:IfcSIUnit
       {:Dimensions "", :UnitType :SOLIDANGLEUNIT, :Name :STERADIAN}}
      {:IfcSIUnit {:Dimensions "", :UnitType :MASSUNIT, :Name :GRAM}}
      {:IfcSIUnit {:Dimensions "", :UnitType :TIMEUNIT, :Name :SECOND}}
      {:IfcSIUnit
       {:Dimensions "",
        :UnitType :THERMODYNAMICTEMPERATUREUNIT,
        :Name :DEGREE_CELSIUS}}
      {:IfcSIUnit
       {:Dimensions "",
        :UnitType :LUMINOUSINTENSITYUNIT,
        :Name :LUMEN}}
      {:IfcSIUnit
       {:Dimensions "", :UnitType :PLANEANGLEUNIT, :Name :RADIAN}}
      {:IfcConversionBasedUnit
       {:Dimensions
        {:IfcDimensionalExponents
         {:LengthExponent 0,
          :MassExponent 0,
          :TimeExponent 0,
          :ElectricCurrentExponent 0,
          :ThermodynamicTemperatureExponent 0,
          :AmountOfSubstanceExponent 0,
          :LuminousIntensityExponent 0}},
        :UnitType :PLANEANGLEUNIT,
        :Name "DEGREE",
        :ConversionFactor
        {:IfcMeasureWithUnit
         {:ValueComponent {:IfcPlaneAngleMeasure 0.01745},
          :UnitComponent
          {:IfcSIUnit
           {:Dimensions "",
            :UnitType :PLANEANGLEUNIT,
            :Name :RADIAN}}}}}}]}},
   :OwnerHistory
   {:IfcOwnerHistory
    {:OwningUser
     {:IfcPersonAndOrganization
      {:ThePerson
       {:IfcPerson
        {:Identification "ikeough",
         :FamilyName "keough",
         :GivenName "ian",
         :Roles [{:IfcActorRole {:Role :ARCHITECT}}]}},
       :TheOrganization
       {:IfcOrganization
        {:Identification "hypar",
         :Name "hypar",
         :Description "Hypar - Generative design for AEC.",
         :Addresses
         [{:IfcPostalAddress
           {:Town "Gotham",
            :AddressLines ("12345 Sesame Street."),
            :Purpose :OFFICE,
            :Region "NY",
            :Description "Office",
            :Country "USA",
            :PostalCode "10005"}}]}}}},
     :OwningApplication
     {:IfcApplication
      {:ApplicationDeveloper {:IfcOrganization {:Name "IFC-dotnet"}},
       :Version "0.1.4.0",
       :ApplicationFullName "IFC-dotnet",
       :ApplicationIdentifier "IFC-dotnet"}},
     :CreationDate 1571866440}}}})
```
or a flat map:
```clojure
(clojure.pprint/pprint
  (step/ifc-deserialize 
    (slurp "test/ifc_tools_clj/samples/project.ifc")
    :flat))
```

### Serialization
```clojure
(let [f (slurp "test/ifc_tools_clj/samples/project.ifc")
      parsed (ifc-deserialize f)]
  (ifc-serialize parsed))
```

### Tests
```shell script
bin/kaocha
```


## TODO

- Converter step/xml/rdf
- Diff tool
- mvdXML validation

## License

Copyright © 2019-2020 Shtanglitza.

Available under the terms of the Eclipse Public License 2.0, see `LICENSE`.