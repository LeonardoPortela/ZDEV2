*DATA lw_components         TYPE ty_generic_list.
*DATA lv_ordem_carregamento TYPE aufk-ordemcarreg.
*DATA lt_stpo               TYPE TABLE OF stpo_api02.
*
*DATA(_services) = NEW zcl_pp_services( ).
*DATA: cd_mapa      TYPE atwrt,
*      lv_component TYPE matnr18.
*
*_services->select_shipment(
*  EXPORTING
*  ordem_carregamento = ordem_carregamento
*  EXCEPTIONS
*  shipment_not_found = 4
*).
*
*IF ( sy-subrc IS INITIAL ).
*  DATA(_shipment) = _services->get_shipment( ).
*
*  "//Seleciona Empresa/nome
*  SELECT SINGLE butxt
*    FROM t001
*    INTO gw_order_header-empresa
*   WHERE bukrs = _shipment-bukrs.
*
*  "//Seleciona Filial/Nome
*  SELECT SINGLE name
*    FROM j_1bbranch
*    INTO gw_order_header-filial
*   WHERE bukrs  = _shipment-bukrs
*     AND branch = _shipment-werks.
*
*  "//Seleciona cliente/nome
*  SELECT SINGLE a~kunnr b~name1
*    FROM vbak AS a
*   INNER JOIN kna1 AS b ON a~kunnr = b~kunnr
*    INTO (gw_order_header-cliente,
*          gw_order_header-nome )
*   WHERE vbeln = _shipment-kdauf.
*
*  "//Seleciona pedido
*  SELECT SINGLE bstkd
*    FROM vbkd
*    INTO gw_order_header-pedido
*   WHERE vbeln = _shipment-kdauf.
*
*  SELECT SINGLE name_text
*    FROM v_username
*    INTO gw_order_header-username
*   WHERE bname = sy-uname.
*
*  _services->select_material(
*   EXPORTING
*    material = _shipment-matnr
*   IMPORTING
*    t_mara = DATA(_mara)
*    t_makt = DATA(_makt)
*  ).
*
*  PERFORM get_cd_mapa
*    USING cd_mapa
*          registro_mapa
*          _shipment-ordemcarreg.
*
**  gw_order_header-registro_mapa      = _mara-groes.
*  gw_order_header-registro_mapa      = cd_mapa.
*  gw_order_header-ordem_producao     = _shipment-aufnr.
*  gw_order_header-ordem_carregamento = _shipment-ordemcarreg.
*  gw_order_header-data               = _shipment-erdat.
*  gw_order_header-lote               = _shipment-aufnr.
*  gw_order_header-material           = _shipment-matnr.
*  gw_order_header-texto              = _makt-maktx.
*  gw_order_header-quantidade         = _shipment-psmng.
*  gw_order_header-placa              = _shipment-placa.
*
*  "--- //OBTEM TEORES DO MATERIAL ACABADO
*  PERFORM get_teor_real
*    USING _shipment-matnr
*          _shipment-werks
*          gw_formulation
*          _services.
*
*  gw_formulation-menge = gw_order_header-quantidade.
*  gw_formulation-idnrk = gw_order_header-material.
*  gw_formulation-descr = gw_order_header-texto.
*  "-- //END
*
*  "--//OBTEM TEORES DAS MATÉRIAS PRIMAS
*  PERFORM get_components
*   TABLES
*    lt_stpo
*   USING
*    _shipment-matnr
*    _shipment-werks
*    _shipment-stlal.
*
*  SELECT SINGLE rsnum
*    FROM afko
*    INTO @DATA(vl_rsnum)
*    WHERE aufnr EQ @gw_order_header-ordem_producao.
*
*  SELECT *
*    FROM resb
*    INTO TABLE @DATA(it_resb)
*    WHERE rsnum EQ @vl_rsnum.
*
*  LOOP AT lt_stpo INTO DATA(lw_stpo).
*    DATA component TYPE makt-matnr.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = lw_stpo-component
*      IMPORTING
*        output = component.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = lw_stpo-component
*      IMPORTING
*        output = lv_component.
*
*    READ TABLE it_resb INTO DATA(wa_resb) WITH KEY
*    posnr = lw_stpo-item_no
*    matnr = component.
*    IF sy-subrc IS NOT INITIAL.
*      READ TABLE it_resb INTO wa_resb WITH KEY
*      posnr = lw_stpo-item_no
*      matnr = lv_component.
*    ENDIF.
*
*    SELECT SINGLE *
*      FROM makt
*      INTO @DATA(lw_makt)
*     WHERE matnr = @component.
*    IF sy-subrc IS NOT INITIAL.
*      SELECT SINGLE *
*        FROM makt
*        INTO lw_makt
*       WHERE matnr = lv_component.
*    ENDIF.
*
*    PERFORM get_teor_real
*      USING lw_stpo-component _shipment-werks lw_components _services.
*
*    PERFORM get_teor_nominal
*      USING lw_stpo-component _shipment-werks lw_components _services.
*
*    IF lw_stpo-fixed_qty IS NOT INITIAL.
*      lw_components-total = gw_order_header-quantidade.
*    ELSE.
*      lw_components-total = wa_resb-bdmng.
*    ENDIF.
*
*    lw_components-idnrk = lw_stpo-component.
*    lw_components-descr = lw_makt-maktx.
*    lw_components-menge = lw_stpo-comp_qty.
*    APPEND lw_components TO gt_components.
*
*    CLEAR: lw_components, wa_resb.
*  ENDLOOP.
*  "-- //END
*ENDIF.
