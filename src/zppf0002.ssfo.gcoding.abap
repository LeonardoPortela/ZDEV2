DATA lt_texto_sd  TYPE TABLE OF tline.
DATA lw_mara      TYPE mara.
DATA: cd_mapa TYPE atwrt.

DATA(_services) = NEW zcl_pp_services( ).

IF ( ordem_carregamento IS NOT INITIAL ).
  CALL METHOD _services->select_shipment
    EXPORTING
      ordem_carregamento = ordem_carregamento
    EXCEPTIONS
      shipment_not_found = 4.

  IF ( sy-subrc IS INITIAL ).
    DATA(_shipment) = _services->get_shipment( ).

    CALL METHOD _services->select_material
      EXPORTING
        material = _shipment-matnr
      IMPORTING
        t_mara   = lw_mara.

    CALL METHOD zcl_pp_services=>get_text_sd
      EXPORTING
        matnr = _shipment-matnr    " Nº do material
        werks = _shipment-werks    " Centro
      IMPORTING
        text  = lt_texto_sd.   " Categoria de tabela para estrutura Tline


  PERFORM get_cd_mapa
      USING cd_mapa
            _shipment-werks
            _shipment-matnr
            ordem_carregamento.

*    PERFORM read_text_sd
*     TABLES lt_texto_sd
*      USING _shipment-matnr _shipment-werks.

    PERFORM get_teor_real
      USING lw_mara-matnr
            _shipment-werks
            gw_etiqueta
            _services.

    TRY.
        gw_etiqueta-textosd_01 = lt_texto_sd[ 1 ]-tdline.
        gw_etiqueta-textosd_02 = lt_texto_sd[ 2 ]-tdline.
        gw_etiqueta-textosd_03 = lt_texto_sd[ 3 ]-tdline.
        gw_etiqueta-textosd_04 = lt_texto_sd[ 4 ]-tdline.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    DATA(_lote) = _shipment-aufnr.
    SHIFT _lote LEFT DELETING LEADING '0'.

    gw_etiqueta-placa          = _shipment-placa.
    gw_etiqueta-undefined      = '- -'.
    gw_etiqueta-registro_mapa  = cd_mapa.
    gw_etiqueta-lote           = _lote.
    gw_etiqueta-qte_ton        = _shipment-psmng / 1000.
    gw_etiqueta-fabricado_em   = _shipment-erdat.
    gw_etiqueta-prazo_validade = _shipment-erdat + lw_mara-mhdhb.

    DO qtd_etiquetas TIMES.
      APPEND gw_etiqueta TO gt_etiqueta.
    ENDDO.
  ENDIF.

ELSE.

  CALL METHOD _services->select_material
    EXPORTING
      material = material
    IMPORTING
      t_mara   = lw_mara.

  PERFORM get_teor_real
    USING lw_mara-matnr
          _shipment-werks
          gw_etiqueta
          _services.

  CALL METHOD zcl_pp_services=>get_text_sd
    EXPORTING
      matnr = material    " Nº do material
      werks = _shipment-werks    " Centro
    IMPORTING
      text  = lt_texto_sd.   " Categoria de tabela para estrutura Tline

*  PERFORM read_text_sd
*   TABLES lt_texto_sd
*    USING material _shipment-werks.

  PERFORM get_cd_mapa
      USING cd_mapa
            _shipment-werks
            material
            ordem_carregamento.

  TRY.
      gw_etiqueta-textosd_01 = lt_texto_sd[ 1 ]-tdline.
      gw_etiqueta-textosd_02 = lt_texto_sd[ 2 ]-tdline.
      gw_etiqueta-textosd_03 = lt_texto_sd[ 3 ]-tdline.
      gw_etiqueta-textosd_04 = lt_texto_sd[ 4 ]-tdline.
    CATCH cx_sy_itab_line_not_found.
  ENDTRY.

  gw_etiqueta-placa          = '- -'.
  gw_etiqueta-undefined      = '- -'.
  gw_etiqueta-registro_mapa  = cd_mapa.
  gw_etiqueta-lote           = '- -'.
  gw_etiqueta-qte_ton        = '- -'.
  gw_etiqueta-fabricado_em   = '- -'.
  gw_etiqueta-prazo_validade = '- -'.

  DO qtd_etiquetas TIMES.
    APPEND gw_etiqueta TO gt_etiqueta.
  ENDDO.

ENDIF.
