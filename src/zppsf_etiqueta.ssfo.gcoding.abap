DATA lt_texto_sd  TYPE TABLE OF tline.
DATA lw_mara      TYPE mara.
DATA lv_cnpj      type char20.
DATA lv_ie        type char20.
DATA lv_cep       type char20.
DATA lv_rmapa     type char70.
DATA(_services) = NEW zcl_pp_services( ).
DATA lv_centro(10) type n.
DATA: convert TYPE p.
DATA: char_ TYPE c LENGTH 255.
DATA: cd_mapa  TYPE atwrt.
DATA: cd_mapah TYPE atwrt.
DATA lv_werks type char10.
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

if _shipment-werks is not initial.

lv_centro = _shipment-werks.
lv_werks = lv_centro.

select single * from lfa1
  into gw_filial
  where lifnr eq lv_werks.
  if sy-subrc is initial.
    gw_etiqueta = CORRESPONDING #( gw_filial ).
endif.

if gw_etiqueta-STCD1 is not initial.
write gw_etiqueta-STCD1 USING EDIT MASK
      '__.___.___/____-__'
      to lv_cnpj.
endif.
   gw_etiqueta-CNPJ_H = |CNPJ: | && lv_cnpj.
*   gw_etiqueta-material = |Material: | && _shipment-matnr.

if gw_etiqueta-stcd3 is not initial.
write gw_etiqueta-stcd3 USING EDIT MASK
      '__.___.___-_'
      to lv_ie.
endif.
   gw_etiqueta-IE_H = |I.E.: | && lv_ie.

if GW_ETIQUETA-ORT02 is not initial.
GW_ETIQUETA-ort02 = GW_ETIQUETA-ORT02.
endif.

GW_ETIQUETA-filial_h = |FILIAL: | && GW_ETIQUETA-lifnr+6(4).

if GW_ETIQUETA-ORT01 is not INITIAL.
GW_ETIQUETA-REGIO_H = GW_ETIQUETA-ORT01 && |/| && GW_ETIQUETA-REGIO.
endif.

GW_ETIQUETA-CEP_H = |CEP: | && GW_ETIQUETA-PSTLZ.

GW_ETIQUETA-CP_H = |CAIXA POSTAL: | && GW_ETIQUETA-PFACH.

  endif.

*    PERFORM read_text_sd
*     TABLES lt_texto_sd
*      USING _shipment-matnr _shipment-werks.

    PERFORM get_cd_mapa
      USING cd_mapa
            _shipment-werks
            material
            ordem_carregamento.

    PERFORM get_teor_real
      USING lw_mara-matnr
            _shipment-werks
            gw_etiqueta
            _services
       CHANGING cd_mapah.

if cd_mapah is not initial.
GW_ETIQUETA-REG_MAP_H = |No.REG.MAPA EP: | && cd_mapah."GW_ETIQUETA-REGISTRO_MAPA.
endif.

    TRY.
        gw_etiqueta-textosd_01 = lt_texto_sd[ 1 ]-tdline.
        gw_etiqueta-textosd_02 = lt_texto_sd[ 2 ]-tdline.
        gw_etiqueta-textosd_03 = lt_texto_sd[ 3 ]-tdline.
        gw_etiqueta-textosd_04 = lt_texto_sd[ 4 ]-tdline.
        gw_etiqueta-textosd_05 = lt_texto_sd[ 5 ]-tdline.
        gw_etiqueta-textosd_06 = lt_texto_sd[ 6 ]-tdline.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    DATA(_lote) = _shipment-aufnr.
    SHIFT _lote LEFT DELETING LEADING '0'.

    gw_etiqueta-placa          = _shipment-placa.
    gw_etiqueta-undefined      = ' - - '.
*    gw_etiqueta-registro_mapa  = lw_mara-groes.
    gw_etiqueta-registro_mapa  = cd_mapa.
    gw_etiqueta-lote           = _lote.

if cd_mapah is not initial.
GW_ETIQUETA-REG_MAP_H = |No.REG.MAPA EP: | && |{ cd_mapah }|."GW_ETIQUETA-REGISTRO_MAPA.
endif.

    convert = 0.
    convert = _shipment-psmng.
    WRITE convert TO char_.
    CONDENSE char_ NO-GAPS.
    gw_etiqueta-qte_ton = char_.

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

  PERFORM get_cd_mapa
    USING cd_mapa
*          cd_mapah
          centro
          material
          ordem_carregamento.

  PERFORM get_teor_real
    USING lw_mara-matnr
          centro
          gw_etiqueta
          _services
    CHANGING cd_mapah.

  CALL METHOD zcl_pp_services=>get_text_sd
    EXPORTING
      matnr = material    " Nº do material
      werks = centro    " Centro
    IMPORTING
      text  = lt_texto_sd.   " Categoria de tabela para estrutura Tline

select SINGLE * from lfa1
  into gw_filial
  where lifnr eq centro.
  if sy-subrc is initial.
    gw_etiqueta = CORRESPONDING #( gw_filial ).

*    gw_etiqueta-STCD1
if cd_mapah is not initial.
GW_ETIQUETA-REG_MAP_H = |No.REG.MAPA EP: | && |{ cd_mapah }|."GW_ETIQUETA-REGISTRO_MAPA.
endif.

  endif.

*  PERFORM read_text_sd
*   TABLES lt_texto_sd
*    USING material centro.

  TRY.
      gw_etiqueta-textosd_01 = lt_texto_sd[ 1 ]-tdline.
      gw_etiqueta-textosd_02 = lt_texto_sd[ 2 ]-tdline.
      gw_etiqueta-textosd_03 = lt_texto_sd[ 3 ]-tdline.
      gw_etiqueta-textosd_04 = lt_texto_sd[ 4 ]-tdline.
      gw_etiqueta-textosd_05 = lt_texto_sd[ 5 ]-tdline.
      gw_etiqueta-textosd_06 = lt_texto_sd[ 6 ]-tdline.
    CATCH cx_sy_itab_line_not_found.
  ENDTRY.

  gw_etiqueta-placa          = ' - - '.
  gw_etiqueta-undefined      = ' - - '.
*  gw_etiqueta-registro_mapa  = lw_mara-groes.
  gw_etiqueta-registro_mapa  = cd_mapa.
  gw_etiqueta-lote           = ' - - '.
  gw_etiqueta-qte_ton        = ' - - '.
  gw_etiqueta-fabricado_em   = ' - - '.
  gw_etiqueta-prazo_validade = ' - - '.

if cd_mapah is not initial.
GW_ETIQUETA-REG_MAP_H = |No.REG.MAPA EP: | && |{ cd_mapah }|."GW_ETIQUETA-REGISTRO_MAPA.
endif.

  DO qtd_etiquetas TIMES.
    APPEND gw_etiqueta TO gt_etiqueta.
  ENDDO.
ENDIF.
