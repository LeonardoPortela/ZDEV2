FORM get_teor_real USING material
                         werks
                         table TYPE ty_etiqueta
                         model TYPE REF TO zcl_pp_services.

  DATA: convert  TYPE p DECIMALS 2,
        number   TYPE p LENGTH 2 DECIMALS 2,
        vl_class TYPE klah-class VALUE 'GARANTIA_FERT'.

  vl_class = |{ vl_class }_{ werks ALPHA = IN }|.

  model->get_component_detail(
  EXPORTING
    material = material
    classe   = vl_class
    initial_charact = abap_true
  IMPORTING
    data = DATA(_teor_real)
  EXCEPTIONS
    data_not_found = 4 ).

  LOOP AT _teor_real INTO DATA(lw_teor_real).

    CHECK lw_teor_real-atnam(3) EQ 'F01'.

    IF lw_teor_real-ausp1 = '?'.
      lw_teor_real-ausp1 = '- -'.
    ENDIF.

    convert = 0.

    REPLACE ALL OCCURRENCES OF ',' IN lw_teor_real-ausp1 WITH '.'.
    IF lw_teor_real-ausp1 NE '- -'.
      convert = CONV #( lw_teor_real-ausp1 ).
      lw_teor_real-ausp1 = CONV #( convert ).
      REPLACE ALL OCCURRENCES OF '.' IN lw_teor_real-ausp1 WITH ','.
      CONDENSE lw_teor_real-ausp1 NO-GAPS.
    ENDIF.

    CASE lw_teor_real-atnam+9(21).
      WHEN 'N_REAL'.    "/NITROGENIO
        table-nitrogenio = lw_teor_real-ausp1.
      WHEN 'PCNA_REAL'. "/FOSFORO
        table-fosforo = lw_teor_real-ausp1.
      WHEN 'K_REAL'.    "/POTASSIO
        table-potassio = lw_teor_real-ausp1.
      WHEN 'PH2O_REAL'. "/AGUA
        table-agua = lw_teor_real-ausp1.
    ENDCASE.

  ENDLOOP.
ENDFORM.

FORM read_text_sd TABLES text TYPE STANDARD TABLE USING
                    material TYPE mara-matnr
                    werks.

  SELECT SINGLE sprsl
    FROM t002t
    INTO @DATA(vl_lang)
    WHERE sptxt EQ @werks
      AND spras EQ @sy-langu.

  SELECT SINGLE *
  FROM mvke
  INTO @DATA(lw_mvke)
 WHERE matnr = @material.

  DATA name_obj TYPE thead-tdname.
  name_obj = lw_mvke-matnr && lw_mvke-vkorg && lw_mvke-vtweg.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = '0001'
      language                = vl_lang
      name                    = name_obj
      object                  = 'MVKE'
    TABLES
      lines                   = text
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

*
*  DATA wa_line  TYPE tline.
*  DATA: tl_texto TYPE catsxt_longtext_itab.
*
*  CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
*    EXPORTING
*      im_title = 'Texto para Etiqueta'
*    CHANGING
*      ch_text  = tl_texto.
*
*  LOOP AT tl_texto INTO DATA(wa_texto).
*    wa_line-tdline = wa_texto.
*    APPEND wa_line TO text.
*  ENDLOOP.

ENDFORM.

FORM get_cd_mapa USING cd_mapa TYPE atwrt
                       p_werks TYPE aufk-werks
                       material TYPE matnr
                       ordem_carregamento TYPE aufk-ordemcarreg.

  DATA(_services) = NEW zcl_pp_services( ).
  DATA id_mapa TYPE atwrt.

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
     material = COND #( WHEN material IS INITIAL THEN _shipment-matnr ELSE material )
     classe   = vl_class
     initial_charact = abap_true
    IMPORTING
     data = DATA(_cd_mapa)
    EXCEPTIONS
     data_not_found = 4 ).

  id_mapa = |F04_{ _shipment-werks }_CD_MAPA|.

  READ TABLE _cd_mapa INTO DATA(wl_cd_mapa) WITH KEY atnam = id_mapa.
  cd_mapa = wl_cd_mapa-ausp1.

ENDFORM.
