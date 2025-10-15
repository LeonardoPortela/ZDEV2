FORM get_teor_real USING material
                         werks
                         table TYPE ty_generic_list
                         model TYPE REF TO zcl_pp_services.
  DATA: vr_ausp1 TYPE c LENGTH 6.
  DATA: vl_class TYPE klah-class VALUE 'GARANTIA_FERT'.

  vl_class = |{ vl_class }_{ werks ALPHA = IN }|.

  CALL METHOD model->get_component_detail
    EXPORTING
      material       = material
      classe         = vl_class
    IMPORTING
      data           = DATA(_teor_real)
    EXCEPTIONS
      data_not_found = 4.

  LOOP AT _teor_real INTO DATA(lw_teor_real).

    CHECK lw_teor_real-atnam(3) EQ 'F01' .

    CLEAR: vr_ausp1.
    vr_ausp1 = lw_teor_real-ausp1.
    CONDENSE vr_ausp1 NO-GAPS.

    CASE lw_teor_real-atnam+9(21).
      WHEN 'N_REAL'.    "/NITROGENIO
        table-nitrogenio = lw_teor_real-ausp1.
      WHEN 'PCNA_REAL'. "/FOSFORO
        table-fosforo = lw_teor_real-ausp1.
      WHEN 'K_REAL'.    "/POTASSIO
        table-potassio = lw_teor_real-ausp1.
      WHEN 'ZN_REAL'.   "/ZINCO
        table-zinco = lw_teor_real-ausp1.
      WHEN 'B_REAL'.    "/BORO
        table-boro = lw_teor_real-ausp1.
      WHEN 'MO_REAL'.   "/MOLIBDENIO
        table-molibdenio = lw_teor_real-ausp1.
      WHEN 'CU_REAL'.   "/COBRE
        table-cobre = lw_teor_real-ausp1.
      WHEN 'MG_REAL'.   "/MAGNESIO
        table-magnesio = lw_teor_real-ausp1.
      WHEN 'CA_REAL'.   "/CALCIO
        table-calcio = lw_teor_real-ausp1.
      WHEN 'ZTEOR_S_REAL'.    "/ENXOFRE
        table-enxofre = lw_teor_real-ausp1.
      WHEN 'S_SO4_REAL'."/ENXOFRE SO4
        table-enxofre_so4 = lw_teor_real-ausp1.
      WHEN 'PH2O_REAL'. "/AGUA
        table-agua = lw_teor_real-ausp1.
      WHEN 'MN_REAL'.    "//MANGANÊS
        table-manganes = lw_teor_real-ausp1.
      WHEN 'PENE_48_REAL'. "//  Peneira 4,8mm
        table-peneira_48 = vr_ausp1.
      WHEN 'PENE_20_REAL'. "//  Peneira 2,0mm
        table-peneira_20 = vr_ausp1.
      WHEN 'PENE_10_REAL'.   "//  Peneira 1,0mm
        table-peneira_10 = vr_ausp1.
    ENDCASE.

  ENDLOOP.
ENDFORM.

FORM get_teor_nominal USING material
                            werks
                            table TYPE ty_generic_list
                            model TYPE REF TO zcl_pp_services.
  DATA: vr_ausp1 TYPE c LENGTH 6.
  DATA: vl_class TYPE klah-class VALUE 'GARANTIA_FERT'.

  vl_class = |{ vl_class }_{ werks ALPHA = IN }|.

  model->get_component_detail(
                         EXPORTING
                          material = material
                          classe   = vl_class
                         IMPORTING
                           data = DATA(_teor_nominal)
                          EXCEPTIONS
                            data_not_found = 4
                        ).

  LOOP AT _teor_nominal INTO DATA(lw_teor_nominal).

    CHECK lw_teor_nominal-atnam(3) EQ 'F02'.

    CLEAR: vr_ausp1.
    vr_ausp1 = lw_teor_nominal-ausp1.
    CONDENSE vr_ausp1 NO-GAPS.

    CASE lw_teor_nominal-atnam+9(21).
      WHEN 'N_NOMINAL'.    "/NITROGENIO
        table-nitrogenio = |{ table-nitrogenio }/{ lw_teor_nominal-ausp1 }|.
      WHEN 'PCNA_NOMINAL'. "/FOSFORO
        table-fosforo = |{ table-fosforo }/{ lw_teor_nominal-ausp1 }|.
      WHEN 'K_NOMINAL'.    "/POTASSIO
        table-potassio = |{ table-potassio }/{ lw_teor_nominal-ausp1 }|.
      WHEN 'ZN_NOMINAL'.   "/ZINCO
        table-zinco = |{ table-zinco }/{ lw_teor_nominal-ausp1 }|.
      WHEN 'B_NOMINAL'.    "/BORO
        table-boro = |{ table-boro }/{ lw_teor_nominal-ausp1 }|.
      WHEN 'MO_NOMINAL'.   "/MOLIBDENIO
        table-molibdenio = |{ table-molibdenio }/{ lw_teor_nominal-ausp1 }|.
      WHEN 'CU_NOMINAL'.   "/COBRE
        table-cobre = |{ table-cobre }/{ lw_teor_nominal-ausp1 }|.
      WHEN 'MG_NOMINAL'.   "/MAGNESIO
        table-magnesio = |{ table-magnesio }/{ lw_teor_nominal-ausp1 }|.
      WHEN 'CA_NOMINAL'.   "/CALCIO
        table-calcio = |{ table-calcio }/{ lw_teor_nominal-ausp1 }|.
      WHEN 'S_NOMINAL'.    "/ENXOFRE
        table-enxofre = |{ table-enxofre }/{ lw_teor_nominal-ausp1 }|.
      WHEN 'S_SO4_NOMINAL'. "/ENXOFRE SO4
        table-enxofre_so4 = |{ table-enxofre_so4 }/{ lw_teor_nominal-ausp1 }|.
      WHEN 'PH2O_NOMINAL'. "/AGUA
        table-agua = |{ table-agua }/{ lw_teor_nominal-ausp1 }|.
      WHEN 'MN_NOMINAL'.    "//MANGANÊS
        table-manganes = |{ table-manganes }/{ lw_teor_nominal-ausp1 }|.
      WHEN 'PENE_48_NOMINAL'.    "//Peneira 4,8mm (Nominal)
        table-peneira_48 = |{ table-peneira_48 }/{ vr_ausp1 }|.
      WHEN 'PENE_20_NOMINAL'.    "//Peneira 2,0 mm (Nominal)
        table-peneira_20 = |{ table-peneira_20 }/{ vr_ausp1 }|.
      WHEN 'PENE_10_NOMINAL'.    "//Peneira 1,0 mm (Nominal)
        table-peneira_10 = |{ table-peneira_10 }/{ vr_ausp1 }|.
    ENDCASE.

  ENDLOOP.
ENDFORM.

*FORM GET_COMPONENT_DETAIL TABLES T_OBJECTDATA
*                           USING OBJKEY CLASS.
*
*  DATA LT_CLASS      TYPE TABLE OF SCLASS.
*  "//DATA LT_OBJECTDATA TYPE TABLE OF CLOBJDAT.
*
*  CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
*    EXPORTING
*      CLASS             = CLASS
*      CLASSTYPE         = '001'
*      FEATURES          = 'X'
*      LANGUAGE          = SY-LANGU
*      OBJECT            = OBJKEY
*      OBJECTTABLE       = 'MARA'
*      KEY_DATE          = SY-DATUM
*      INITIAL_CHARACT   = ' '
*    TABLES
*      T_CLASS           = LT_CLASS
*      T_OBJECTDATA      = T_OBJECTDATA
*    EXCEPTIONS
*      NO_CLASSIFICATION = 4
*      NO_CLASSTYPES     = 7.
*ENDFORM.

*FORM CONVERT_COMPONENT_VALUE CHANGING COMPONENT.
*  CALL FUNCTION 'CONVERSION_EXIT_ATINN_OUTPUT '
*    EXPORTING
*      INPUT  = COMPONENT
*    IMPORTING
*      OUTPUT = COMPONENT.
*ENDFORM.

*FORM GET_OBJKEY_COMPONENT USING COMPONENT CHANGING OBJKEY.
*  DATA LV_TABLE TYPE BAPI1003_KEY-OBJECTTABLE VALUE 'MARA'.
*  DATA RETURN   TYPE TABLE OF BAPIRET2.
*
*  SHIFT COMPONENT LEFT DELETING LEADING '0'.
*
*  DATA(_OBJKEY_TABLE) =
*    VALUE TT_BAPI1003_OBJECT_KEYS( ( KEY_FIELD = 'MATNR'
*                                     VALUE_INT = COMPONENT
*                                 ) ).
*
*  CALL FUNCTION 'BAPI_OBJCL_CONCATENATEKEY'
*    EXPORTING
*      OBJECTTABLE    = LV_TABLE
*    IMPORTING
*      OBJECTKEY_CONC = OBJKEY
*    TABLES
*      OBJECTKEYTABLE = _OBJKEY_TABLE
*      RETURN         = RETURN.
*ENDFORM.

FORM get_components TABLES components TYPE STANDARD TABLE
                     USING material centro lista_alternativa.

  DATA warning TYPE capiflag-flwarning.

  CALL FUNCTION 'CSAP_MAT_BOM_READ'
    EXPORTING
      material    = material
      plant       = centro
      bom_usage   = '1'
      alternative = lista_alternativa
    IMPORTING
      fl_warning  = warning
    TABLES
      t_stpo      = components.

ENDFORM.

FORM get_cd_mapa USING cd_mapa TYPE atwrt
                       registro_mapa
                       ordem_carregamento TYPE aufk-ordemcarreg.

  DATA(_services) = NEW zcl_pp_services( ).
  DATA id_mapa TYPE atwrt.
  DATA rg_mapa TYPE atwrt.

  CHECK ordem_carregamento IS NOT INITIAL.

  CALL METHOD _services->select_shipment
    EXPORTING
      ordem_carregamento = ordem_carregamento
    EXCEPTIONS
      shipment_not_found = 4.

  IF sy-subrc IS INITIAL..
    DATA(_shipment) = _services->get_shipment( ).
  ENDIF.

  DATA: vl_class TYPE klah-class VALUE 'GARANTIA_FERT'.

  vl_class = |{ vl_class }_{ _shipment-werks ALPHA = IN }|.

  _services->get_component_detail(
    EXPORTING
     material = _shipment-matnr
     classe   = vl_class
     initial_charact = abap_true
    IMPORTING
     data = DATA(_cd_mapa)
    EXCEPTIONS
     data_not_found = 4 ).

  id_mapa = |F04_{ _shipment-werks }_CD_MAPA|.
  rg_mapa = |F05_{ _shipment-werks }_RG_MAPA|.

  READ TABLE _cd_mapa INTO DATA(wl_cd_mapa) WITH KEY atnam = id_mapa.
  cd_mapa = wl_cd_mapa-ausp1.

  READ TABLE _cd_mapa INTO DATA(wl_rg_mapa) WITH KEY atnam = rg_mapa.
  registro_mapa = wl_rg_mapa-ausp1.

ENDFORM.
