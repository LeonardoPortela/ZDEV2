class ZCL_MM_UTIL definition
  public
  final
  create public .

public section.

  methods GET_CARACTERISTICA_MATERIAL
    importing
      !I_OBJECT type AUSP-OBJEK
      !I_CLASS type KLAH-CLASS
    exporting
      !T_OBJECTDATA type TT_CLOBJDAT .
  methods GET_CODIGO_INDEA
    importing
      !I_MATERIAL type MATNR18
    exporting
      !ID_INDEA type ZSDT0210-ID_MATNR_IDEA .
  methods GET_CARACTERISTICA_LOTE
    importing
      !I_OBJECT type AUSP-OBJEK
      !I_CLASS type KLAH-CLASS
      !I_CLASSTYPE type KLASSENART
      !I_OBJECTTABLE type TABELLE
    exporting
      !T_OBJECTDATA type TT_CLOBJDAT .
  methods GET_RENASEM
    importing
      !I_MATERIAL type MATNR18
    exporting
      !R_RENASEM type ZSDT0216-RENASEM .
  methods GET_PESO_BAG
    importing
      !I_MATERIAL type MATNR18
    exporting
      !R_PESO_BAG type KWMENG .
  methods GET_NEXT_ID_LOG
    returning
      value(ID) type ZSDT0214-ID .
  methods GET_CARACTERISTICA_GERAL
    importing
      !I_CLASS type KLASSE_D default 'SEMENTES'
      !I_CLASSTYPE type KLASSENART default '023'
      !I_OBJECT type CUOBN
      !I_OBJECTTABLE type TABELLE default 'MCH1'
      !I_CARACTERISTICA type ATNAM
    exporting
      !E_VALOR_CARACTERISTICA type ATWRT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MM_UTIL IMPLEMENTATION.


  METHOD GET_CARACTERISTICA_LOTE.

    DATA: T_CLASS TYPE TABLE OF SCLASS.

    CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
      EXPORTING
        CLASS              = I_CLASS
        CLASSTYPE          = I_CLASSTYPE
        OBJECT             = I_OBJECT
        OBJECTTABLE        = I_OBJECTTABLE
      TABLES
        T_CLASS            = T_CLASS
        T_OBJECTDATA       = T_OBJECTDATA
      EXCEPTIONS
        NO_CLASSIFICATION  = 1
        NO_CLASSTYPES      = 2
        INVALID_CLASS_TYPE = 3
        OTHERS             = 4.

*SEMENTE_MARCAS
*SEMENTE_RENASCEM
*SEMENTE_ATESTADO
*SEMENTE_BOLETIM
*SEMENTE_GERMINACAO
*SEMENTE_PUREZA
*SEMENTE_DATAANÁLISE
*SEMENTE_DATAVALIDADE
*SEMENTE_ESPECIE
*SAFRA_CONJUGADA
*SEMENTE_CATEGORIA
*PESO_BAG
*PENEIRA
*POSICAO
*PILHA
*CERTIFICADO
*TERMO
*SAFRA
*SEMENTE_MARCA
*CLASSE

  ENDMETHOD.


  METHOD get_caracteristica_material.

    DATA: t_class TYPE TABLE OF sclass.

    CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
      EXPORTING
        class              = i_class
        classtype          = '001'
        object             = i_object
        objecttable        = 'MCHA'
      TABLES
        t_class            = t_class
        t_objectdata       = t_objectdata
      EXCEPTIONS
        no_classification  = 1
        no_classtypes      = 2
        invalid_class_type = 3
        OTHERS             = 4.

  ENDMETHOD.


  METHOD GET_CODIGO_INDEA.

    DATA: LV_OBJECT TYPE AUSP-OBJEK.

    LV_OBJECT = I_MATERIAL.

    ME->GET_CARACTERISTICA_MATERIAL( EXPORTING I_CLASS      = 'SEMENTES_GERAL'
                                               I_OBJECT     = LV_OBJECT
                                     IMPORTING T_OBJECTDATA = DATA(LT_OBJECTDATA) ).

    SORT LT_OBJECTDATA BY ATNAM.

    READ TABLE LT_OBJECTDATA ASSIGNING FIELD-SYMBOL(<FS_DATA>)
    WITH KEY ATNAM = 'ID_CULTIVAR_INDEA'
    BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      REPLACE '.' IN <FS_DATA>-AUSP1 WITH ''.
      ID_INDEA = <FS_DATA>-AUSP1.
    ENDIF.

  ENDMETHOD.


  METHOD get_next_id_log.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZSEQ_L_IND'
      IMPORTING
        number                  = id
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
  ENDMETHOD.


  METHOD GET_PESO_BAG.
    DATA: lv_object TYPE ausp-objek.

    lv_object = i_material.

    me->get_caracteristica_material( EXPORTING i_class = 'SEMENTES_GERAL' i_object = lv_object
                                     IMPORTING t_objectdata = DATA(lt_objectdata) ).

    SORT lt_objectdata BY atnam.

    READ TABLE lt_objectdata ASSIGNING FIELD-SYMBOL(<fs_data>)
    WITH KEY atnam = 'PESO_BAG'
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      r_peso_bag = <fs_data>-ausp1.
    ENDIF.

  ENDMETHOD.


  METHOD GET_RENASEM.
    DATA: lv_object TYPE ausp-objek.

    lv_object = i_material.

    me->get_caracteristica_material( EXPORTING i_class = 'SEMENTES_GERAL' i_object = lv_object
                                     IMPORTING t_objectdata = DATA(lt_objectdata) ).

    SORT lt_objectdata BY atnam.

    READ TABLE lt_objectdata ASSIGNING FIELD-SYMBOL(<fs_data>)
    WITH KEY atnam = 'SEMENTE_RENASCEM'
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      r_renasem = <fs_data>-ausp1.
    ENDIF.

  ENDMETHOD.


  METHOD get_caracteristica_geral.

    FREE: e_valor_caracteristica.

    CALL METHOD get_caracteristica_lote
      EXPORTING
        i_object      = i_object      "// 000000000000446042                      AP403525
        i_class       = i_class       "// SEMENTES
        i_classtype   = i_classtype   "// 023
        i_objecttable = i_objecttable "// MCH1
      IMPORTING
        t_objectdata  = DATA(lt_objectdata).

    SORT lt_objectdata BY atnam.

    READ TABLE lt_objectdata INTO DATA(ls_data) WITH KEY atnam = i_caracteristica.

    CHECK sy-subrc IS INITIAL.

    CASE i_caracteristica.
      WHEN 'PESO_BAG'.
        REPLACE '.' IN ls_data-ausp1 WITH ''.
        REPLACE ',' IN ls_data-ausp1 WITH '.'.
        REPLACE 'kg' IN ls_data-ausp1 WITH ''.
        REPLACE 'KG' IN ls_data-ausp1 WITH ''.
        CONDENSE ls_data-ausp1 NO-GAPS.
      WHEN OTHERS.
    ENDCASE.

    e_valor_caracteristica = ls_data-ausp1.

  ENDMETHOD.
ENDCLASS.
