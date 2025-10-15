class ZCL_FRETE_REMESSA_TRANS definition
  public
  final
  create public .

public section.

  interfaces ZIF_FRETE_REMESSA_TRANS .

  data CABECALHO type ZDE_BAPI_REMESSA_CAB_T .
  data ITEMS type ZDE_BAPI_REMESSA_ITEM_T .
  data PARCEIROS type ZDE_BAPI_REMESSA_PARCEIROS_T .
  data PARTICAO_LOTE type CHAR01 .
  data GERAR_MOVIMENTO type CHAR01 .
  data AV_VBELN type VBELN_VL .
  data AT_VFDAT type VFDAT .
  data ITENS type ZIB_NFE_DIST_ITM_T .
  data VOLUMES_TRANSP type ZIB_NFE_DIST_TVO_T .
  data LOTES type ZIB_NFE_DIST_LOT_T .
  data LOTES_GERAL type ZIB_NFE_DIST_LOT_T .
  data LOTES_CARACTERISTICAS type ZIB_NFE_DIST_LCA_T .
  data RETORNO type BAPIRET2_T .
  data CK_ALTEROU_LOTES type CHAR01 .
  data CK_ALTEROU type CHAR01 .

  methods SET_LIMPAR .
  methods SET_GERAR_REMESSA
    importing
      !I_TIPO_DELIVERY type CHAR01 optional
      !I_CABECALHO type ZDE_BAPI_REMESSA_CAB_T
      !I_ITEMS type ZDE_BAPI_REMESSA_ITEM_T
      !I_PARCEIROS type ZDE_BAPI_REMESSA_PARCEIROS_T
      !I_GERAR_MOVIMENTO type CHAR01 optional
      !I_PARTICAO_LOTE type CHAR01 optional
    exporting
      !E_DADOS_REMESSA type ZZSDT_DADOS_REMESSA
    returning
      value(R_GEROU) type CHAR01
    raising
      ZCX_DELIVERY .
  methods SET_CRIAR_ENTRADA
    exporting
      !E_DADOS_REMESSA type ZZSDT_DADOS_REMESSA
    returning
      value(R_GEROU) type CHAR01
    raising
      ZCX_DELIVERY .
  methods SET_CRIAR_REMESSA
    importing
      !T_ITENS_PED type ZIB_NFE_DIST_ITM_T
      !T_LOTES_PED type ZIB_NFE_DIST_LOT_T
    exporting
      !E_DADOS_REMESSA type ZZSDT_DADOS_REMESSA
      !E_LOG_REMESSA type ZLEST0247_T
    returning
      value(R_GEROU) type CHAR01 .
  methods SET_NR_REMESSA
    importing
      !I_REMESSA type VBELN_VL
    returning
      value(R_DELIVERY) type ref to ZCL_FRETE_REMESSA_TRANS .
  methods SET_RETORNO
    importing
      !I_RETORNO type BAPIRET2_T .
  methods ADD_LOTE_ITEM
    importing
      !I_CHAVE_NFE type ZDE_CHAVE_DOC_E
      !I_PROD_ITEM type J_1BITMNUM
    exporting
      !E_CARACTERISTICAS type ZIB_NFE_DIST_LCA_T
    returning
      value(R_LOTE) type ZIB_NFE_DIST_LOT
    raising
      ZCX_NFE_INBOUND_EXCEPTION
      ZCX_CHARG_EXCEPTION
      ZCX_CADASTRO .
  methods ADD_ITEM
    importing
      !I_ITEM type ZDE_NFE_DIST_ITM_ALV .
  methods SET_LOTE_ITEM
    changing
      !I_LOTE_CARACT type ZIB_NFE_DIST_LCA_T
      !I_LOTE type ZIB_NFE_DIST_LOT .
  methods ADD_LOTE
    importing
      !I_LOTE type ZIB_NFE_DIST_LOT_T .
  methods GET_INFO_NOTA
    returning
      value(R_NFE_INBOUND) type ZNFE_INBOUND .
  methods EXCLUIR_LOTE_ITEM
    importing
      !I_CD_LOTE_ITEM type ZDE_CD_LOTE_ITEM .
  methods SET_ELIMINA_REMESSA
    importing
      !I_VBELN type VBELN_VL .
  methods SET_PIKING
    importing
      !T_ITENS type ZDE_BAPI_REMESSA_ITEM_T
    exporting
      !E_LOG_REMESSA type ZLEST0247_T
    returning
      value(R_GEROU) type CHAR01
    raising
      ZCX_DELIVERY .
protected section.
private section.
ENDCLASS.



CLASS ZCL_FRETE_REMESSA_TRANS IMPLEMENTATION.


  METHOD zif_frete_remessa_trans~get_dados_transporte.

    FREE: e_placa,     e_quantidade, e_tp_frete, e_itinerario,
          e_vlr_frete, e_unid_cond,  e_dados_transp,
          e_lock_ag_frete.

    DATA v_tknum TYPE vttk-tknum.

*-----------------------------------
* recupera informacoes
*-----------------------------------
    SELECT *
      INTO @DATA(w_zlest0211)
      FROM zlest0211
        UP TO 1 ROWS
     WHERE vbeln = @i_rem_vbeln
       AND ebeln = @i_ebeln
       AND ebelp = @i_ebelp.

    ENDSELECT.

    IF w_zlest0211 IS INITIAL. "recupera standard
      SELECT SINGLE vttk~tknum INTO v_tknum
      FROM vbfa INNER JOIN vttk ON  vttk~tknum = vbfa~vbeln

     WHERE vbfa~vbelv    = i_rem_vbeln
       AND vbfa~vbtyp_n  = '8'
       AND vbfa~vbtyp_v  = 'J'.

      SELECT SINGLE tdlnr, text1
        INTO ( @w_zlest0211-agente_frete, @w_zlest0211-placa_cav )
        FROM vttk
        WHERE tknum = @v_tknum.
      "
      SELECT SINGLE netwr,waers
          FROM vfkp INTO ( @w_zlest0211-vlr_frete, @w_zlest0211-unid_cond )
         WHERE rebel = @v_tknum.

    ENDIF.


    SELECT ktokk
      INTO @DATA(l_ktokk)
      FROM lfa1
        UP TO 1 ROWS
     WHERE lifnr = @w_zlest0211-agente_frete.
    ENDSELECT.


    SELECT *
      INTO TABLE @DATA(t_likp)
      FROM likp
     WHERE vbeln = @i_rem_vbeln.

    SELECT lfimg, gewei
      INTO TABLE @DATA(t_lips)
      FROM lips
     WHERE vbeln = @i_rem_vbeln.


*-----------------------------------
* atribuir informacoes
*-----------------------------------
    READ TABLE t_likp INTO DATA(w_likp) INDEX 1.

    DATA(l_quant) = 0.
    LOOP AT t_lips INTO DATA(w_lips).
      l_quant     = l_quant + w_lips-lfimg.
    ENDLOOP.

    e_tp_frete       = w_likp-inco1.
    e_placa          = w_zlest0211-placa_cav.
    e_quantidade     = l_quant.
    e_itinerario     = w_likp-route.
    e_vlr_frete      = w_zlest0211-vlr_frete.
    e_unid_cond      = w_zlest0211-unid_cond.

*   IF e_tp_frete <> 'CIF'.              "*-CS2024000522-29.08.2024-JT-#150113-inicio
*     e_dados_transp = icon_icon_list.   "*-CS2024000522-29.08.2024-JT-#150113-inicio
*   ELSE.
    IF w_zlest0211 IS INITIAL.
      e_dados_transp = icon_import_transport_request.
    ELSE.
      c_ag_frete     = w_zlest0211-agente_frete.
      e_dados_transp = icon_transport.
    ENDIF.
*   ENDIF.

    IF w_zlest0211-agente_frete IS NOT INITIAL.
      e_lock_ag_frete = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_frete_remessa_trans~get_instance.

    IF zif_frete_remessa_trans~at_frete_remessa_trans IS NOT BOUND.
      CREATE OBJECT zif_frete_remessa_trans~at_frete_remessa_trans TYPE zcl_frete_remessa_trans.
      r_instancia = zif_frete_remessa_trans~at_frete_remessa_trans.
    ELSE.
      r_instancia = zif_frete_remessa_trans~at_frete_remessa_trans.
    ENDIF.
  ENDMETHOD.


  METHOD zif_frete_remessa_trans~get_status_outros_docs.

    DATA: l_icon         TYPE icon-id,
          lv_doc_custo   TYPE fknum,
          lv_ordem_serv	 TYPE vbeln_va,
          lv_fatura_serv TYPE vbeln_vf,
          lv_dacte       TYPE j_1bdocnum,
          lv_st_proc     TYPE zst_proc.

    FREE:  e_doc_custo,  e_ordem_serv,  e_fatura_serv,  e_dacte,
          lv_doc_custo, lv_ordem_serv, lv_fatura_serv, lv_dacte,
          lv_st_proc.


    l_icon = icon_icon_list.
*--------------------------
*-- documentos gerados
*--------------------------
    SELECT fknum, ov_frete, fatura_frete, nro_nf_frete, doc_transp
      FROM zlest0211
      INTO @DATA(w_zlest0211)
        UP TO 1 ROWS
     WHERE vbeln = @i_rem_vbeln
       AND ebeln = @i_ebeln
       AND ebelp = @i_ebelp.
    ENDSELECT.


    IF sy-subrc NE 0.
      SELECT SINGLE vttk~tknum INTO  w_zlest0211-doc_transp
      FROM vbfa INNER JOIN vttk ON  vttk~tknum = vbfa~vbeln
     WHERE vbfa~vbelv    = i_rem_vbeln
       AND vbfa~vbtyp_n  = '8'
       AND vbfa~vbtyp_v  = 'J'.
    ENDIF.


    IF w_zlest0211-doc_transp is INITIAL.
      e_doc_custo     = icon_icon_list.
      e_ordem_serv    = icon_icon_list.
      e_fatura_serv   = icon_icon_list.
      e_dacte         = icon_icon_list.
      EXIT.
    ENDIF.

*--------------------------
*-- DOC.CUSTO
*--------------------------
    IF w_zlest0211-fknum IS INITIAL.
      SELECT vbeln
        FROM vtfa
        INTO @DATA(l_fknum)
          UP TO 1 ROWS
       WHERE tknum   = @space
         AND vbelv   = @w_zlest0211-doc_transp
         AND vbtyp_v = '8'.
      ENDSELECT.
      IF sy-subrc = 0.
        e_doc_custo  = l_fknum.
      ELSE.
        e_doc_custo  = icon_icon_list.
      ENDIF.
    ELSE.
      SELECT fknum
        INTO l_fknum
        FROM vfkk
          UP TO 1 ROWS
       WHERE fknum = w_zlest0211-fknum.
      ENDSELECT.

      IF sy-subrc = 0.
        e_doc_custo  = w_zlest0211-fknum.
      ELSE.
        e_doc_custo  = icon_icon_list.
      ENDIF.
    ENDIF.

*--------------------------
*-- OV Servico
*--------------------------
    IF w_zlest0211-ov_frete IS INITIAL.
      IF w_zlest0211-doc_transp IS NOT INITIAL.
        SELECT vbeln
          INTO @DATA(l_ov_frete)
          FROM vbak
            UP TO 1 ROWS
         WHERE tknum = @w_zlest0211-doc_transp.
        ENDSELECT.
      ELSE.
        sy-subrc = 4.
      ENDIF.

      IF sy-subrc = 0.
        e_ordem_serv = l_ov_frete.
      ELSE.
        e_ordem_serv = l_icon.
      ENDIF.
    ELSE.
      SELECT vbeln
        INTO l_ov_frete
        FROM vbak
          UP TO 1 ROWS
       WHERE vbeln = w_zlest0211-ov_frete.
      ENDSELECT.

      IF sy-subrc = 0.
        e_ordem_serv = w_zlest0211-ov_frete.
      ELSE.
        e_ordem_serv = l_icon.
      ENDIF.
    ENDIF.

*--------------------------
*---- Farura Servico
*--------------------------
    IF w_zlest0211-fatura_frete IS INITIAL.
      SELECT vbeln
        FROM vbfa
        INTO @DATA(l_fatura_serv)
          UP TO 1 ROWS
       WHERE vbelv   = @e_ordem_serv
         AND vbtyp_n = 'M'
         AND vbtyp_v = 'C'.
      ENDSELECT.

      IF sy-subrc = 0.
        e_fatura_serv = l_fatura_serv.
      ELSE.
        e_fatura_serv = l_icon.
      ENDIF.
    ELSE.
      SELECT vbeln
        INTO l_fatura_serv
        FROM vbrk
          UP TO 1 ROWS
       WHERE vbeln = w_zlest0211-fatura_frete.
      ENDSELECT.

      IF sy-subrc = 0.
        SELECT vbeln, mjahr
          FROM vbfa
          INTO @DATA(w_vbfa)
            UP TO 1 ROWS
         WHERE vbelv    = @w_zlest0211-fatura_frete
           AND vbtyp_n  = 'N'. "estorno
        ENDSELECT.

        IF sy-subrc = 0.
          e_fatura_serv = l_icon.
        ELSE.
          e_fatura_serv = w_zlest0211-fatura_frete.
        ENDIF.
      ELSE.
        e_fatura_serv = l_icon.
      ENDIF.
    ENDIF.

*--------------------------
*-- DACTE
*--------------------------
    IF w_zlest0211-nro_nf_frete IS INITIAL.
      SELECT docnum
        INTO @DATA(l_docnum)
        FROM j_1bnflin
          UP TO 1 ROWS
       WHERE reftyp = 'BI'
         AND refkey = @e_fatura_serv.
      ENDSELECT.

      IF sy-subrc = 0.
        e_dacte       = l_docnum.
      ELSE.
        e_dacte       = l_icon.
      ENDIF.
    ELSE.
      SELECT bukrs, docnum, cancel
        INTO @DATA(w_doc)
        FROM j_1bnfdoc
          UP TO 1 ROWS
       WHERE docnum = @w_zlest0211-nro_nf_frete.
      ENDSELECT.

      IF sy-subrc = 0.
        IF w_doc-cancel = abap_true.
          e_dacte       = l_icon.
        ELSE.
          e_dacte       = w_zlest0211-nro_nf_frete.
        ENDIF.
      ELSE.
        e_dacte       = l_icon.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD zif_frete_remessa_trans~get_status_vt.
    FREE: e_transp.

*--------------------------
*-- checa se tem erro processamento
*--------------------------
    SELECT vbeln
      INTO @DATA(l_vbeln)
      FROM zlest0214
        UP TO 1 ROWS
     WHERE vbeln      = @i_rem_vbeln
       AND etapa_proc = @zif_frete_remessa_trans~c_etapa_gerar_vt
       AND msgtyp     = @zif_frete_remessa_trans~c_e.
    ENDSELECT.

    IF sy-subrc = 0.
      e_transp = icon_message_error.
      UPDATE zlest0211 SET doc_transp = abap_off
                           st_proc    = '03'
                     WHERE vbeln = i_rem_vbeln.
    ELSE.
      SELECT vbeln
        INTO l_vbeln
        FROM vbfa
          UP TO 1 ROWS
       WHERE vbelv   = i_rem_vbeln
         AND vbtyp_n = zif_frete_remessa_trans~c_8
         AND vbtyp_v = zif_frete_remessa_trans~c_j.
      ENDSELECT.
*
      IF sy-subrc = 0.
        e_transp = l_vbeln.
        UPDATE zlest0211 SET doc_transp = l_vbeln
                             st_proc    = '04'
                       WHERE vbeln      = i_rem_vbeln.
      ELSE.
        e_transp = icon_execute_object.
        UPDATE zlest0211 SET doc_transp = abap_off
                             st_proc    = '03'
                       WHERE vbeln      = i_rem_vbeln.
      ENDIF.
    ENDIF.

    COMMIT WORK.
  ENDMETHOD.


  METHOD zif_frete_remessa_trans~get_tab_return.
    t_return[] = zif_frete_remessa_trans~at_t_return[].
  ENDMETHOD.


  METHOD zif_frete_remessa_trans~set_calcula_frete.
    DATA: l_add01    TYPE zde_info_frete-add01,
          l_cont_fre TYPE i,
          l_mensagem TYPE char255,
          t_a900     TYPE TABLE OF a900,
          w_a900     TYPE a900,
          t_a910     TYPE TABLE OF a910,
          w_a910     TYPE a910,
          t_a911     TYPE TABLE OF a911,
          w_a911     TYPE a911,
          t_a915     TYPE TABLE OF a915,
          w_a915     TYPE a915,
          t_a918     TYPE TABLE OF a918,
          w_a918     TYPE a918,
          t_a919     TYPE TABLE OF a919,
          w_a919     TYPE a919,
          t_a942     TYPE TABLE OF a942,
          w_a942     TYPE a942,
          t_konp     TYPE TABLE OF konp,
          w_konp     TYPE konp,
          t_return   TYPE TABLE OF bapiret2.

    DATA: vg_tipo_frete(3).

    CLEAR: w_a900, w_a910, w_a911, w_a915, w_a918, w_a919,
           l_cont_fre,
           l_add01,
           e_valor_frete,
           e_unid_cond.

*--------------------------------------
*-- log de proc
*--------------------------------------
    zif_frete_remessa_trans~set_free_return( ).
    zif_frete_remessa_trans~set_free_log_proc( EXPORTING i_vbeln = i_rem_vbeln
                                                         i_ebeln = i_ebeln
                                                         i_ebelp = i_ebelp
                                                      i_etapa_proc = zif_frete_remessa_trans~c_etapa_calculo_frete ).


*--------------------------------------
*-- remessa
*--------------------------------------
    SELECT matnr
      INTO @DATA(l_matnr)
      FROM lips
        UP TO 1 ROWS
     WHERE vbeln = @i_rem_vbeln.
    ENDSELECT.

    SELECT route, inco1                      "*-CS2024000522-18.07.2024-JT-#143588-inicio
      FROM likp
      INTO (@DATA(l_route), @vg_tipo_frete)  "*-CS2024000522-18.07.2024-JT-#143588-inicio
        UP TO 1 ROWS
     WHERE vbeln = @i_rem_vbeln.
    ENDSELECT.

    SELECT shtyp
      INTO @DATA(l_shtyp)
      FROM zsdt0011
        UP TO 1 ROWS
     WHERE bsart  = @i_ped_bsart
       AND tp_movimento  = 'S'.

    ENDSELECT.

*--------------------------------------
*-- informacoes transporte
*--------------------------------------
    SELECT *
      INTO @DATA(w_zlest0211)
      FROM zlest0211
        UP TO 1 ROWS
      WHERE vbeln = @i_rem_vbeln
        AND ebeln = @i_ebeln
        AND ebelp = @i_ebelp.

    ENDSELECT.

*--------------------------------------
*-- fornecedor
*--------------------------------------
    SELECT *
      INTO @DATA(w_lfa1)
      FROM lfa1
        UP TO 1 ROWS
      WHERE lifnr = @w_zlest0211-cod_loc_coleta. "LC
    ENDSELECT.

*   CLEAR: vg_tipo_frete.         "*-CS2024000522-18.07.2024-JT-#143588-inicio
    SELECT SINGLE *
     FROM lfa1
     INTO @DATA(wlfa1)
     WHERE lifnr = @i_agente_frete.

*   IF wlfa1-ktokk = 'ZFIC'.      "*-CS2024000522-18.07.2024-JT-#143588-inicio
*     vg_tipo_frete = 'CIF'.
*   ELSE.
*     vg_tipo_frete = 'CPT'.
*   ENDIF.

*--------------------------------------
*-- cliente
*--------------------------------------
    SELECT *
      INTO @DATA(w_kna1)
      FROM kna1
        UP TO 1 ROWS
      WHERE kunnr = @w_zlest0211-cod_loc_entrega.  "LR
    ENDSELECT.

*--------------------------------------
*-- informacoes transporte
*--------------------------------------
    IF vg_tipo_frete  <> 'CPT'.

      TRY.
          zcl_veiculos=>zif_veiculos~get_instance(
             )->set_veiculo(
                EXPORTING
                  i_placa         = w_zlest0211-placa_cav
             )->get_tipo_contrato(
                IMPORTING
                  e_tipo_contrato = l_add01
             ).

        CATCH zcx_veiculos INTO DATA(ex_veiculo).    "
          ex_veiculo->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'S' ).

          l_mensagem = 'Erro ao localizar ADD01.Placa:' && w_zlest0211-placa_cav.

          zif_frete_remessa_trans~set_tab_return( i_type = 'E' i_id = 'SD' i_number = '024' i_message = l_mensagem ).
          t_return = zif_frete_remessa_trans~get_tab_return( ).
          zif_frete_remessa_trans~set_criar_log( EXPORTING  i_vbeln      = i_rem_vbeln
                                                            i_ebeln = i_ebeln
                                                            i_ebelp = i_ebelp
                                                            i_etapa_proc = zif_frete_remessa_trans~c_etapa_calculo_frete
                                                            i_commit     = abap_true
                                                   CHANGING t_return     = t_return[] ).

          RAISE EXCEPTION TYPE zcx_frete_remessa_trans
            EXPORTING
              textid = VALUE #( msgid  = zcx_frete_remessa_trans=>zcx_erro_geral-msgid
                                msgno  = zcx_frete_remessa_trans=>zcx_erro_geral-msgno
                                attr1  = CONV #( sy-msgv1 ) attr2  = CONV #( sy-msgv2 ) attr3  = CONV #( sy-msgv3 ) attr4  = CONV #( sy-msgv4 ) )
              msgty  = 'E'
              msgid  = zcx_frete_remessa_trans=>zcx_erro_geral-msgid
              msgno  = zcx_frete_remessa_trans=>zcx_erro_geral-msgno
              msgv1  = sy-msgv1
              msgv2  = sy-msgv2
              msgv3  = sy-msgv3
              msgv4  = sy-msgv4.
      ENDTRY.
    ENDIF.

*--------------------------------------
*-- calculo frete
*--------------------------------------
    SELECT SINGLE * INTO w_a900 FROM a900 AS a WHERE shtyp  = l_shtyp
                                                 AND tdlnr  = i_agente_frete
                                                 AND route  = l_route
                                                 AND add01  = l_add01
                                                 AND datab LE sy-datum
                                                 AND datbi GE sy-datum
                                                 AND EXISTS ( SELECT b~knumh FROM konp AS b WHERE b~knumh = a~knumh AND b~loevm_ko EQ '' ).
    IF sy-subrc = 0.
      CLEAR: w_konp.
      SELECT SINGLE * INTO w_konp FROM konp WHERE knumh = w_a900-knumh.
      IF sy-subrc = 0.
        e_valor_frete = w_konp-kbetr.
        e_unid_cond   = w_konp-konwa.
        ADD 1 TO l_cont_fre.
      ENDIF.
    ENDIF.

    SELECT SINGLE * INTO w_a910 FROM a910 AS a WHERE shtyp   = l_shtyp
                                                 AND tdlnr   = i_agente_frete
                                                 AND lzonea  = w_lfa1-lzone
                                                 AND lzonez  = w_kna1-lzone
                                                 AND datab  LE sy-datum
                                                 AND datbi  GE sy-datum
                                                 AND EXISTS ( SELECT b~knumh FROM konp AS b WHERE b~knumh = a~knumh AND b~loevm_ko EQ '' ).
    IF sy-subrc = 0.
      CLEAR: w_konp.
      SELECT SINGLE * INTO w_konp FROM konp WHERE knumh = w_a910-knumh.
      IF sy-subrc = 0.
        e_valor_frete = w_konp-kbetr.
        e_unid_cond   = w_konp-konwa.
        ADD 1 TO l_cont_fre.
      ENDIF.
    ENDIF.

    SELECT SINGLE * INTO w_a911 FROM a911 AS a WHERE shtyp   = l_shtyp
                                                 AND tdlnr   = i_agente_frete
                                                 AND route   = l_route
                                                 AND datab  LE sy-datum
                                                 AND datbi  GE sy-datum
                                                 AND EXISTS ( SELECT b~knumh FROM konp AS b WHERE b~knumh = a~knumh AND b~loevm_ko EQ '' ).
    IF sy-subrc = 0.
      CLEAR: w_konp.
      SELECT SINGLE * INTO w_konp FROM konp WHERE knumh = w_a911-knumh.
      IF sy-subrc = 0.
        e_valor_frete = w_konp-kbetr.
        e_unid_cond   = w_konp-konwa.
        ADD 1 TO l_cont_fre.
      ENDIF.
    ENDIF.

    SELECT SINGLE * INTO w_a915 FROM a915 AS a WHERE shtyp   = l_shtyp
                                                 AND tdlnr   = i_agente_frete
                                                 AND lzonea  = w_lfa1-lzone
                                                 AND lzonez  = w_kna1-lzone
                                                 AND add01   = l_add01
                                                 AND datab  LE sy-datum
                                                 AND datbi  GE sy-datum
                                                 AND EXISTS ( SELECT b~knumh FROM konp AS b WHERE b~knumh = a~knumh AND b~loevm_ko EQ '' ).
    IF sy-subrc = 0.
      CLEAR: w_konp.
      SELECT SINGLE * INTO w_konp FROM konp WHERE knumh = w_a915-knumh.
      IF sy-subrc = 0.
        e_valor_frete = w_konp-kbetr.
        e_unid_cond   = w_konp-konwa.
        ADD 1 TO l_cont_fre.
      ENDIF.
    ENDIF.

    SELECT SINGLE * INTO w_a918 FROM a918 AS a WHERE shtyp   = l_shtyp
                                                 AND tdlnr   = i_agente_frete
                                                 AND matnr   = l_matnr
                                                 AND lzonea  = w_lfa1-lzone
                                                 AND lzonez  = w_kna1-lzone
                                                 AND add01   = l_add01
                                                 AND datab  LE sy-datum
                                                 AND datbi  GE sy-datum
                                                 AND EXISTS ( SELECT b~knumh FROM konp AS b WHERE b~knumh = a~knumh AND b~loevm_ko EQ '' ).
    IF sy-subrc = 0.
      CLEAR: w_konp.
      SELECT SINGLE * INTO w_konp FROM konp WHERE knumh = w_a918-knumh.
      IF sy-subrc = 0.
        e_valor_frete = w_konp-kbetr.
        e_unid_cond   = w_konp-konwa.
        ADD 1 TO l_cont_fre.
      ENDIF.
    ENDIF.

    SELECT SINGLE * INTO w_a919 FROM a919 AS a WHERE shtyp  = l_shtyp
                                                 AND tdlnr  = i_agente_frete
                                                 AND matnr  = l_matnr
                                                 AND lzonea = w_lfa1-lzone
                                                 AND lzonez = w_kna1-lzone
                                                 AND datab  LE sy-datum
                                                 AND datbi  GE sy-datum
                                                 AND EXISTS ( SELECT b~knumh FROM konp AS b WHERE b~knumh = a~knumh AND b~loevm_ko EQ '' ).
    IF sy-subrc = 0.
      CLEAR: w_konp.
      SELECT SINGLE * INTO w_konp FROM konp WHERE knumh = w_a919-knumh.
      IF sy-subrc = 0.
        e_valor_frete = w_konp-kbetr.
        e_unid_cond   = w_konp-konwa.
        ADD 1 TO l_cont_fre.
      ENDIF.
    ENDIF.

    IF e_valor_frete IS INITIAL.
      zif_frete_remessa_trans~set_tab_return( i_type = 'W' i_id = 'SD' i_number = '024' i_message = 'Frete não foi calculado' ).
      t_return = zif_frete_remessa_trans~get_tab_return( ).
      zif_frete_remessa_trans~set_criar_log( EXPORTING  i_vbeln      = i_rem_vbeln
                                                        i_ebeln = i_ebeln
                                                        i_ebelp = i_ebelp
                                                        i_etapa_proc = zif_frete_remessa_trans~c_etapa_calculo_frete
                                                        i_commit     = abap_true
                                               CHANGING t_return     = t_return[] ).
    ELSE.
      l_mensagem = 'Frete calculado. Valor R$' && e_valor_frete.
      zif_frete_remessa_trans~set_tab_return( i_type = 'S' i_id = 'SD' i_number = '024' i_message = l_mensagem ).
      t_return = zif_frete_remessa_trans~get_tab_return( ).
      zif_frete_remessa_trans~set_criar_log( EXPORTING i_vbeln      = i_rem_vbeln
                                                       i_ebeln = i_ebeln
                                                       i_ebelp = i_ebelp
                                                       i_etapa_proc = zif_frete_remessa_trans~c_etapa_calculo_frete
                                                       i_commit     = abap_true
                                               CHANGING t_return     = t_return[] ).


    ENDIF.

  ENDMETHOD.


  METHOD zif_frete_remessa_trans~set_criar_log.
    DATA: w_zlest0214 TYPE zlest0214,
          l_timestamp TYPE timestampl,
          l_message   TYPE char200.

*--------------------------
*-- elimina msg duplicadas
*--------------------------
    SORT t_return BY type id number.
    DELETE t_return WHERE type <> 'E'.
    DELETE ADJACENT DUPLICATES FROM t_return
                          COMPARING type id number.

*--------------------------
*-- criar log
*--------------------------
    LOOP AT t_return INTO DATA(w_return).

      DO.
        GET TIME STAMP FIELD l_timestamp.

        SELECT vbeln
          INTO @DATA(l_vbeln)
          FROM zlest0214
            UP TO 1 ROWS
         WHERE vbeln = @i_vbeln
          AND  ebeln = @i_ebeln
          AND  ebelp = @i_ebelp
          AND  cont  = @l_timestamp.
        ENDSELECT.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
      ENDDO.

      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = w_return-id
          msgnr               = w_return-number
          msgv1               = w_return-message_v1
          msgv2               = w_return-message_v2
          msgv3               = w_return-message_v3
          msgv4               = w_return-message_v4
        IMPORTING
          message_text_output = l_message.

      CLEAR w_zlest0214.
      w_zlest0214-mandt = sy-mandt.
      w_zlest0214-vbeln = i_vbeln.
      w_zlest0214-ebeln = i_ebeln.
      w_zlest0214-ebelp = i_ebelp.
      w_zlest0214-cont        = l_timestamp.
      w_zlest0214-etapa_proc  = i_etapa_proc.
      w_zlest0214-doc_gerado  = i_doc_gerado.
      w_zlest0214-msgtyp      = w_return-type.
      w_zlest0214-msgspra     = sy-langu.
      w_zlest0214-msgid       = w_return-id.
      w_zlest0214-msgnr       = w_return-number.
      w_zlest0214-msgv1       = l_message.
      w_zlest0214-data        = sy-datum.
      w_zlest0214-hora        = sy-uzeit.
      w_zlest0214-usuario     = sy-uname.

      MODIFY zlest0214     FROM w_zlest0214.
    ENDLOOP.

    IF i_commit = abap_true.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDMETHOD.


  METHOD zif_frete_remessa_trans~set_elimina_transporte.

    DATA: t_lips TYPE STANDARD TABLE OF lips.

    SELECT *
      INTO TABLE t_lips
      FROM lips
     WHERE vbeln = i_rem_vbeln.

    READ TABLE t_lips INTO DATA(w_lips_aux)  WITH KEY vbeln = i_rem_vbeln.

    DELETE FROM zlest0211 WHERE vbeln = i_rem_vbeln AND  ebeln = w_lips_aux-vgbel AND ebelp = w_lips_aux-vgpos.
    DELETE FROM zlest0212 WHERE vbeln = i_rem_vbeln.
    DELETE FROM zlest0213 WHERE vbeln = i_rem_vbeln.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD zif_frete_remessa_trans~set_free_log_proc.
    IF i_etapa_proc IS INITIAL.
      DELETE FROM zlest0214 WHERE vbeln = i_vbeln
                             AND  ebeln	=	i_ebeln
                             AND  ebelp = i_ebelp
                             AND msgtyp = zif_frete_remessa_trans~c_e.
    ELSE.
      DELETE FROM zlest0214 WHERE ebeln      = i_vbeln
                             AND  ebeln      =  i_ebeln
                             AND  ebelp      = i_ebelp
                             AND  etapa_proc = i_etapa_proc.
    ENDIF.

    COMMIT WORK AND WAIT.
  ENDMETHOD.


  METHOD zif_frete_remessa_trans~set_free_return.
    FREE: zif_frete_remessa_trans~at_t_return[].
  ENDMETHOD.


  METHOD zif_frete_remessa_trans~set_limpa_erros_log.
    CHECK t_vbeln_transf[] IS NOT INITIAL.

    SELECT *
      FROM zlest0214
      INTO TABLE @DATA(t_0228)
       FOR ALL ENTRIES IN @t_vbeln_transf
     WHERE vbeln  = @t_vbeln_transf-vbeln
      AND ebeln   = @t_vbeln_transf-ebeln
      AND ebelp   = @t_vbeln_transf-ebelp.

    DELETE t_0228 WHERE msgtyp <> 'E'.

    CHECK t_0228[] IS NOT INITIAL.

    SORT t_0228 BY vbeln ebeln ebelp etapa_proc.
    DELETE ADJACENT DUPLICATES FROM t_0228
                          COMPARING vbeln ebeln ebelp etapa_proc.

    LOOP AT t_0228 INTO DATA(w_0228).
      DELETE FROM zlest0214 WHERE vbeln      = w_0228-vbeln
                              AND ebeln      = w_0228-ebeln
                              AND ebelp      = w_0228-ebelp
                              AND etapa_proc = w_0228-etapa_proc.

    ENDLOOP.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD zif_frete_remessa_trans~set_salva_transporte.


    TYPES: BEGIN OF ty_lips.
             INCLUDE TYPE lips.
             TYPES: xblnr TYPE ekbe-xblnr.
    TYPES: END OF ty_lips.

    DATA: w_zlest0211  TYPE zlest0211,
          w_zlest0212  TYPE zlest0212,
          w_zlest0213  TYPE zlest0213,
          w_jnad       TYPE j_1bnfnad,
          w_lfa1       TYPE lfa1,
          w_campos_nfe TYPE zde_campos_nfe,
          w_campos_cta TYPE zde_campos_nfe,
          l_vlr_frete  TYPE kbetr_kond,
          l_unid_cond  TYPE konwa,
          zcl_util     TYPE REF TO zcl_util.

    DATA: t_lips TYPE STANDARD TABLE OF ty_lips,
          w_lips TYPE ty_lips.

    CREATE OBJECT zcl_util.

    SELECT *
      INTO TABLE @DATA(t_vbpa)
      FROM vbpa
     WHERE vbeln = @i_rem_vbeln.

    SELECT *
      INTO TABLE t_lips
      FROM lips
     WHERE vbeln = i_rem_vbeln.

    LOOP AT t_lips  INTO w_lips.
      w_lips-xblnr = w_lips-vbeln.
      MODIFY t_lips FROM w_lips INDEX sy-tabix.
    ENDLOOP.

    SELECT *
      INTO TABLE @DATA(t_ekbe)
      FROM ekbe
      FOR ALL ENTRIES IN @t_lips
     WHERE ebeln    =  @t_lips-vgbel
       AND ebelp    =  @t_lips-vgpos+1(5)
       AND vgabe    = '6'
       AND bewtp    = 'U'
       AND bwart    = '862'
       AND xblnr    = @t_lips-xblnr.

    IF t_ekbe[] IS NOT INITIAL.

      READ TABLE t_ekbe INTO DATA(lw_ekbe) INDEX 1.
      CONCATENATE lw_ekbe-belnr  lw_ekbe-gjahr INTO DATA(lva_refkey).

      SELECT *
        INTO TABLE @DATA(t_j_1bnflin)
        FROM j_1bnflin
       WHERE refkey  = @lva_refkey.

      SELECT *
        INTO TABLE @DATA(t_j_1bnfdoc)
        FROM j_1bnfdoc
        FOR ALL ENTRIES IN @t_j_1bnflin
      WHERE docnum = @t_j_1bnflin-docnum.

      SELECT *
        INTO TABLE @DATA(t_j_1bnfstx)
      FROM j_1bnfstx
        FOR ALL ENTRIES IN @t_j_1bnfdoc
        WHERE docnum = @t_j_1bnfdoc-docnum.

      SELECT *
        INTO TABLE @DATA(t_j_1bnfe_active)
     FROM j_1bnfe_active
        FOR ALL ENTRIES IN @t_j_1bnfdoc
        WHERE docnum = @t_j_1bnfdoc-docnum.
    ENDIF.

    SELECT *
      INTO TABLE @DATA(t_likp)
      FROM likp
     WHERE vbeln = @i_rem_vbeln.
*
*    SELECT *
*      INTO @DATA(w_zlest0210)
*      FROM zlest0210
*        UP TO 1 ROWS
*     WHERE remessa_dummy = @i_rem_vbeln.
*    ENDSELECT.

    READ TABLE t_lips INTO DATA(w_lips_aux)  WITH KEY vbeln = i_rem_vbeln.

*----------------------------------------
*-- elimina dados antigos
*----------------------------------------
    DELETE FROM zlest0211 WHERE vbeln = i_rem_vbeln AND  ebeln = w_lips_aux-vgbel AND ebelp = w_lips_aux-vgpos.
    DELETE FROM zlest0212 WHERE vbeln = i_rem_vbeln.
    DELETE FROM zlest0213 WHERE vbeln = i_rem_vbeln.

*----------------------------------------
*-- gravar tabela zlest0211
*----------------------------------------

    CLEAR w_zlest0211.

    w_zlest0211-mandt      = sy-mandt.
    w_zlest0211-vbeln      = i_rem_vbeln.
    w_zlest0211-posnr      = i_rem_posnr.
    w_zlest0211-peso_bruto =  i_p_bruto.
    w_zlest0211-peso_tara  =  i_p_liquido.

    w_zlest0211-ebeln = w_lips_aux-vgbel.
    w_zlest0211-ebelp = w_lips_aux-vgpos.

    w_zlest0211-agente_frete      = i_ag_frete.

    READ TABLE t_tab_transp INTO DATA(w_tab_transp)
                            WITH KEY tipo_placa = 'Placa Cavalo'.
    IF sy-subrc = 0.
      w_zlest0211-placa_cav       = w_tab_transp-pc_veiculo.
      w_zlest0211-tp_veiculo_cav  = w_tab_transp-tp_veiculo.
    ENDIF.

    READ TABLE t_tab_transp INTO w_tab_transp
                            WITH KEY tipo_placa = 'Placa Car1'.
    IF sy-subrc = 0.
      w_zlest0211-placa_car1      = w_tab_transp-pc_veiculo.
      w_zlest0211-tp_veiculo_car1 = w_tab_transp-tp_veiculo.
    ENDIF.

    READ TABLE t_tab_transp INTO w_tab_transp
                            WITH KEY tipo_placa = 'Placa Car2'.
    IF sy-subrc = 0.
      w_zlest0211-placa_car2      = w_tab_transp-pc_veiculo.
      w_zlest0211-tp_veiculo_car2 = w_tab_transp-tp_veiculo.
    ENDIF.

    READ TABLE t_tab_transp INTO w_tab_transp
                            WITH KEY tipo_placa = 'Placa Car3'.
    IF sy-subrc = 0.
      w_zlest0211-placa_car3      = w_tab_transp-pc_veiculo.
      w_zlest0211-tp_veiculo_car3 = w_tab_transp-tp_veiculo.
    ENDIF.

    w_zlest0211-motorista         = i_cod_motorista.
    w_zlest0211-region            = w_tab_transp-cd_uf.

    READ TABLE t_vbpa INTO DATA(w_vbpa) WITH KEY parvw = 'LR'.
    IF sy-subrc = 0.
      w_zlest0211-cod_remetente   = w_vbpa-kunnr.
    ENDIF.

    READ TABLE t_vbpa INTO w_vbpa WITH KEY parvw = 'WE'.
    IF sy-subrc = 0.
      w_zlest0211-cod_dest_merc   = w_vbpa-kunnr.
    ENDIF.

    READ TABLE t_vbpa INTO w_vbpa WITH KEY parvw = 'PC'.
    IF sy-subrc = 0.
      w_zlest0211-cod_loc_coleta  = w_vbpa-lifnr.
    ENDIF.

    READ TABLE t_vbpa INTO w_vbpa WITH KEY parvw = 'LR'.
    IF sy-subrc = 0.
      w_zlest0211-cod_loc_entrega = w_vbpa-kunnr.
    ENDIF.

    w_zlest0211-safra_ordem_car	= i_nr_safra.
    w_zlest0211-nro_ordem_car   = i_nr_ordem.
    w_zlest0211-id_ordem        = i_id_ordem.

    MODIFY zlest0211           FROM w_zlest0211.


    READ TABLE 	t_j_1bnfdoc INTO DATA(w_j_1bnfdoc) INDEX 1.
*----------------------------------------
*-- gravar tabela zlest0212
*----------------------------------------
    LOOP AT t_lips   INTO w_lips.

      CLEAR w_zlest0212.

      READ TABLE t_likp INTO DATA(w_likp) WITH KEY vbeln  = w_lips-vbeln.

      w_zlest0212-mandt             = sy-mandt.
      w_zlest0212-vbeln             = w_lips-vbeln.
      w_zlest0212-posnr             = w_lips-posnr.
      w_zlest0212-vbeln_dummy       = ''.
      w_zlest0212-posnr_dummy       = ''.
      w_zlest0212-werks             = w_lips-werks.
      w_zlest0212-lgort             = w_lips-lgort.
      w_zlest0212-charg             = w_lips-charg.
      w_zlest0212-kunnr             = w_likp-kunnr.
      w_zlest0212-matnr             = w_lips-matnr.
      w_zlest0212-nfnum             = w_j_1bnfdoc-nfenum.
      w_zlest0212-serie             = w_j_1bnfdoc-series.
      w_zlest0212-qtde_rem          = w_lips-brgew.
      w_zlest0212-unidade           = w_lips-gewei.

      MODIFY zlest0212           FROM w_zlest0212.
    ENDLOOP.

*----------------------------------------
*-- gravar tabela zlest0213
*----------------------------------------
    CLEAR: w_zlest0213.

    w_zlest0213-mandt             = sy-mandt.
    w_zlest0213-vbeln             = i_rem_vbeln.
    w_zlest0213-posnr             = i_rem_posnr.

    w_zlest0213-nfe               = w_j_1bnfdoc-nfe.
    w_zlest0213-modelo            = w_j_1bnfdoc-model.
    w_zlest0213-serie             = w_j_1bnfdoc-series.
    w_zlest0213-numero            = w_j_1bnfdoc-nfenum.
    w_zlest0213-cliente           = w_likp-kunnr.

    READ TABLE 	t_j_1bnflin INTO DATA(w_j_1bnflin) INDEX 1.

    w_zlest0213-werks             = w_j_1bnflin-werks.
    w_zlest0213-lgort             = w_lips-lgort.
    w_zlest0213-charg             = w_j_1bnflin-charg.
    w_zlest0213-dtemissao         = w_j_1bnfdoc-docdat.


    READ TABLE 	t_j_1bnfstx INTO DATA(w_j_1bnfstx) INDEX 1.
    w_zlest0213-vl_bc             = w_j_1bnfstx-base.
    w_zlest0213-vl_icms           = w_j_1bnfstx-taxval.

    w_zlest0213-vl_produtos       = w_j_1bnflin-netwr.
    w_zlest0213-vl_nota_fiscal    = w_j_1bnflin-netwr.
    w_zlest0213-material          = w_j_1bnflin-matnr.
    w_zlest0213-quantidade        = w_j_1bnflin-menge.
    w_zlest0213-unidade           = w_j_1bnflin-meins.
    w_zlest0213-cfop              = w_j_1bnflin-cfop. "w_zib_dist_itm-prod_cfop && 'AA'.

    READ TABLE 	t_j_1bnfe_active INTO DATA(w_j_1bnfe_active) INDEX 1.
    CONCATENATE
    w_j_1bnfe_active-regio
    w_j_1bnfe_active-nfyear
    w_j_1bnfe_active-nfmonth
    w_j_1bnfe_active-stcd1
    w_j_1bnfe_active-model
    w_j_1bnfe_active-serie
    w_j_1bnfe_active-nfnum9
    w_j_1bnfe_active-docnum9
    w_j_1bnfe_active-cdv INTO  w_zlest0213-chave .

    w_zlest0213-name1             = w_j_1bnfdoc-name1.
    w_zlest0213-stcd1             = w_j_1bnfdoc-cgc.
    w_zlest0213-stcd2             = w_j_1bnfdoc-cpf.
    w_zlest0213-partyp            = w_j_1bnfdoc-partyp.
    w_zlest0213-docnum9           = w_j_1bnfe_active-docnum9.
    w_zlest0213-cdv               = w_j_1bnfe_active-cdv.

    IF w_zlest0213-unidade = 'TL' OR w_zlest0213-unidade = 'TON'.
      w_zlest0213-unidade         = 'TO'.
    ENDIF.

    MODIFY zlest0213           FROM w_zlest0213.

    COMMIT WORK AND WAIT.

*----------------------------------------
*-- calcular frete
*----------------------------------------

    TRY.
        l_vlr_frete = zcl_frete_remessa_trans=>zif_frete_remessa_trans~set_calcula_frete(
                         EXPORTING  i_rem_vbeln    = i_rem_vbeln
                                    i_ebeln    	   = w_lips_aux-vgbel
                                    i_ebelp    	   = w_lips_aux-vgpos
                                    i_agente_frete = i_ag_frete
                                    i_ped_bsart    = i_ped_bsart
                                    i_vei_id_ordem = i_id_ordem

                         IMPORTING e_unid_cond      = l_unid_cond ).
      CATCH zcx_frete_remessa_trans INTO DATA(ex_zcx_frete_remessa_trans).
        EXIT.
    ENDTRY.

*----------------------------------------
*-- atualiza frete
*----------------------------------------
    UPDATE zlest0211 SET vlr_frete = l_vlr_frete
                         kbetr     = l_vlr_frete
                         unid_cond = l_unid_cond
                   WHERE vbeln     = i_rem_vbeln
                     AND ebeln     = w_zlest0211-ebeln
                     AND ebelp     = w_zlest0211-ebelp.


    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD zif_frete_remessa_trans~set_tab_return.

    DATA: w_return  TYPE bapiret2.

    w_return-type        = i_type.   "ZIF_FRETE_REMESSA_TRANS~c_e.
    w_return-id          = i_id.     "ZIF_FRETE_REMESSA_TRANS~c_sd.
    w_return-number      = i_number. "ZIF_FRETE_REMESSA_TRANS~c_024.
    w_return-message_v1  = i_message.
    APPEND w_return     TO zif_frete_remessa_trans~at_t_return.
  ENDMETHOD.


  METHOD add_item.

    FREE: me->itens,
          me->lotes,
          me->lotes_caracteristicas,
          me->ck_alterou,
          me->ck_alterou_lotes.

    DATA: w_itens TYPE zib_nfe_dist_itm.

    READ TABLE me->itens INTO DATA(w_it) WITH KEY chave_nfe = i_item-chave_nfe
                                                  prod_item = i_item-prod_item.
    CHECK sy-subrc <> 0.

    MOVE-CORRESPONDING i_item TO w_itens.
    APPEND w_itens            TO me->itens.

  ENDMETHOD.


  METHOD add_lote.

    me->lotes[] = i_lote[].

  ENDMETHOD.


  METHOD add_lote_item.

    DATA: wa_caracterisrica  TYPE zib_nfe_dist_lca,
          lc_item            TYPE zde_cd_lote_item,
          lc_departamento    TYPE REF TO zcl_mm_departamento,
          lc_zcl_nfe_inbound TYPE REF TO zcl_nfe_inbound.

    CLEAR: r_lote, wa_caracterisrica.

    "Busca Grupos do Departamento
    CREATE OBJECT: lc_departamento,
                   lc_zcl_nfe_inbound.

    lc_departamento->zif_cadastro~set_registro( i_id_registro = '9999' ).
    DATA(it_grupos) = lc_departamento->get_grupo_mercadoria( ).
    CLEAR: lc_departamento.

    "Busca Item
    READ TABLE me->itens WITH KEY chave_nfe = i_chave_nfe
                                  prod_item = i_prod_item INTO DATA(wa_item).
    CHECK sy-subrc IS INITIAL.

    LOOP AT me->lotes INTO DATA(wa_lote_saldo) WHERE chave_nfe = i_chave_nfe
                                                 AND prod_item = i_prod_item.
      wa_item-menge = wa_item-menge - wa_lote_saldo-menge.
    ENDLOOP.

    IF wa_item-menge LE 0.
      RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
        EXPORTING
          textid = VALUE #( msgno = zcx_nfe_inbound_exception=>zcx_sem_saldo_particao-msgno
                            msgid = zcx_nfe_inbound_exception=>zcx_sem_saldo_particao-msgid
                            attr1 = CONV #( i_prod_item ) )
          msgty  = 'E'
          msgid  = zcx_nfe_inbound_exception=>zcx_sem_saldo_particao-msgid
          msgno  = zcx_nfe_inbound_exception=>zcx_sem_saldo_particao-msgno
          msgv1  = CONV #( i_prod_item ).
    ENDIF.

    "Busca Material
    SELECT SINGLE * INTO @DATA(wa_mara) FROM mara WHERE matnr EQ @wa_item-matnr.
    CHECK sy-subrc IS INITIAL.

    "Busca Grupo
    READ TABLE it_grupos WITH KEY matkl = wa_mara-matkl INTO DATA(wa_grupo).
    CHECK sy-subrc IS INITIAL.

    "Busca Classe
    SELECT SINGLE * INTO @DATA(wa_klah) FROM klah WHERE clint EQ @wa_grupo-clint.
    CHECK sy-subrc IS INITIAL.

    "Busca item do Pedido de Compra
    SELECT SINGLE * INTO @DATA(wa_eket) FROM eket WHERE ebeln EQ @wa_item-ebeln AND ebelp EQ @wa_item-ebelp.

    "Busca Caracteristicas da Classe
    "Código para administração em lote obrigatória
    IF wa_mara-xchpf EQ abap_true.
      DATA(r_t_objectdata) = lc_zcl_nfe_inbound->get_carateristicas( i_class = wa_klah-class  i_classtype = wa_klah-klart i_object = CONV #( wa_item-matnr ) ).
    ENDIF.

    "Inclui um Lote na Sequencia
    CLEAR: lc_item.
    LOOP AT me->lotes_geral INTO DATA(wa_lote) WHERE cd_lote_item(1) EQ '$'.
      CONCATENATE '0' wa_lote-cd_lote_item+1(9) INTO wa_lote-cd_lote_item.
      IF wa_lote-cd_lote_item GE lc_item.
        ADD 1 TO wa_lote-cd_lote_item.
        lc_item = wa_lote-cd_lote_item.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lc_item
          IMPORTING
            output = lc_item.
      ENDIF.
    ENDLOOP.
    IF lc_item IS INITIAL.
      lc_item = '0000000001'.
    ENDIF.

    "Dados Iniciais do Lote
    CONCATENATE '$' lc_item+1(9) INTO r_lote-cd_lote_item.

*   r_lote-cd_lote_item = wa_item-ebeln+2(8) && wa_item-ebeln+2(8).
    r_lote-chave_nfe = i_chave_nfe.
    r_lote-prod_item = wa_item-prod_item.
    r_lote-matnr     = wa_item-matnr.
*   r_lote-werks     = me->nota-f_tomadora.
    r_lote-clint     = wa_klah-clint.
    r_lote-klart     = wa_klah-klart.
    r_lote-class     = wa_klah-class.
    r_lote-charg     = wa_eket-charg.
    r_lote-menge     = wa_item-menge.

    "Característicias do Lote (Vazias)
    LOOP AT r_t_objectdata INTO DATA(wa_objectdata).
      CLEAR: wa_caracterisrica.
      wa_caracterisrica-cd_lote_item = r_lote-cd_lote_item.

      CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
        EXPORTING
          input  = wa_objectdata-atnam
        IMPORTING
          output = wa_objectdata-atimb.

      wa_caracterisrica-atinn        = wa_objectdata-atimb.
      wa_caracterisrica-atnam        = wa_objectdata-atnam.
      wa_caracterisrica-smbez        = wa_objectdata-smbez.
      wa_caracterisrica-atwrt        = ''.
      APPEND wa_caracterisrica TO me->lotes_caracteristicas.
    ENDLOOP.

    "Retorna Lote e Características
    APPEND r_lote TO me->lotes.
    APPEND r_lote TO me->lotes_geral.

    MOVE me->lotes_caracteristicas[] TO e_caracteristicas[].
    me->ck_alterou_lotes = abap_true.

  ENDMETHOD.


  METHOD excluir_lote_item.

    DELETE me->lotes WHERE cd_lote_item EQ i_cd_lote_item.
    DELETE me->lotes_caracteristicas WHERE cd_lote_item EQ i_cd_lote_item.
    me->ck_alterou_lotes = abap_true.

  ENDMETHOD.


  METHOD get_info_nota.

    CLEAR: r_nfe_inbound.

    MOVE me->itens[]                 TO r_nfe_inbound-nfe_base-itens[].
    MOVE me->lotes[]                 TO r_nfe_inbound-nfe_base-lotes[].
    MOVE me->lotes_caracteristicas[] TO r_nfe_inbound-nfe_base-lotes_c[].

  ENDMETHOD.


  METHOD set_criar_entrada.

    CONSTANTS: lc_delivery TYPE vbeln VALUE '$        1'.

    TYPES: BEGIN OF ty_dados_processa_po,
             ebeln       TYPE ekpo-ebeln,
             ebelp       TYPE ekpo-ebelp,
             menge       TYPE ekpo-menge,
             meins       TYPE ekpo-meins,
             matnr       TYPE ekpo-matnr,
             ped_werks   TYPE ekpo-werks,
             ped_entrega TYPE lfa1-lifnr,
             lgort       TYPE ekpo-lgort,
             charg       TYPE eket-charg,
             ped_coleta  TYPE ekpa-lifn2,
           END   OF ty_dados_processa_po.

    DATA: lc_remessa           TYPE vbeln_vl,
          lc_retorno           TYPE TABLE OF bapiret2,
          wa_retorno           TYPE bapiret2,
          lc_sender_system     TYPE tbdls-logsys,
          wa_parceiros         TYPE zde_bapi_remessa_parceiros,
          lc_a_lzone           TYPE azone,
          lc_l_lzone           TYPE lzone,
          vl_vlr_nf(20)        TYPE c,
          vl_lfimg(20)         TYPE c,
          vl_kcmeng            TYPE komdlgn-kcmeng,
          ls_komdlgn           TYPE komdlgn,
          ls_lfa1              TYPE lfa1,
          ls_kna1              TYPE kna1,
          lc_item_number       TYPE bapiibdlvitem-itm_number,
          items_particionar	   TYPE zde_bapi_remessa_item_t,
          gt_dados_processa_po TYPE TABLE OF ty_dados_processa_po,
          wa_dados_processa_po TYPE ty_dados_processa_po,
*
          sl_vbkok_wa          TYPE vbkok,
          sl_vbpok             TYPE vbpok,
          tl_vbpok             TYPE TABLE OF vbpok,
          tl_prot              TYPE TABLE OF prott,
          sl_prot              TYPE prott,
          wa_dados_remessa     TYPE zzsd_dados_remessa,
          t_item_data          TYPE TABLE OF bapiobdlvitemchg,
          t_item_control       TYPE TABLE OF bapiobdlvitemctrlchg,
          t_item_data_spl      TYPE TABLE OF /spe/bapiobdlvitemchg,
          w_item_data          TYPE bapiobdlvitemchg,
          w_item_control       TYPE bapiobdlvitemctrlchg,
          w_item_data_spl      TYPE /spe/bapiobdlvitemchg,
          w_header_data        TYPE bapiobdlvhdrchg,
          w_header_control     TYPE bapiobdlvhdrctrlchg,
          t_header_partner     TYPE TABLE OF bapidlvpartnerchg,
          w_header_partner     TYPE bapidlvpartnerchg,
          t_retorno2           TYPE TABLE OF bapiret2,
          lc_quantidade        TYPE lfimg,
*
          i_vbsk               TYPE vbsk,
          it_xkomdlgn          TYPE TABLE OF komdlgn,
          wa_xkomdlgn          TYPE komdlgn,
          it_xvbfs             TYPE TABLE OF vbfs,
          it_xvbls             TYPE TABLE OF vbls,
          it_gn_partner        TYPE TABLE OF partner_gn,
          wa_gn_partner        TYPE partner_gn.

    FREE: e_dados_remessa.

    READ TABLE me->cabecalho INTO DATA(wa_cabecalho) INDEX 1.

    IF wa_cabecalho-route IS INITIAL AND wa_cabecalho-ck_route_validar EQ abap_true.

      CASE wa_cabecalho-lc_coleta_partyp.
        WHEN 'C'.
          SELECT SINGLE lzone INTO lc_a_lzone
            FROM kna1
           WHERE kunnr EQ wa_cabecalho-lc_coleta_parid.
        WHEN 'V' OR 'B'.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_cabecalho-lc_coleta_parid
            IMPORTING
              output = wa_cabecalho-lc_coleta_parid.

          SELECT SINGLE lzone INTO lc_a_lzone
            FROM lfa1
           WHERE lifnr EQ wa_cabecalho-lc_coleta_parid.
      ENDCASE.

      CASE wa_cabecalho-lc_entrega_partyp.
        WHEN 'C'.

          SELECT SINGLE lzone INTO lc_l_lzone
            FROM kna1
           WHERE kunnr EQ wa_cabecalho-lc_entrega_parid.

        WHEN 'V' OR 'B'.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_cabecalho-lc_entrega_parid
            IMPORTING
              output = wa_cabecalho-lc_entrega_parid.

          SELECT SINGLE lzone INTO lc_l_lzone
            FROM lfa1
           WHERE lifnr EQ wa_cabecalho-lc_entrega_parid.
      ENDCASE.

      SELECT SINGLE *
        INTO @DATA(wa_trolz)
        FROM trolz
       WHERE aland = 'BR'
         AND azone = @lc_a_lzone
         AND lland = 'BR'
         AND lzone = @lc_l_lzone.

      IF sy-subrc IS INITIAL.
        wa_cabecalho-route = wa_trolz-route.
      ELSE.
        "ZCX_SEM_ITINERARIO	Não Enc.Itinerário p/ Coleta &MSGV1& (Zona: &MSGV2&) Entrega &MSGV3& (Zona: &MSGV4&)!
        RAISE EXCEPTION TYPE zcx_delivery
          EXPORTING
            textid = VALUE #( msgid = zcx_delivery=>zcx_sem_itinerario-msgid
                              msgno = zcx_delivery=>zcx_sem_itinerario-msgno
                              attr1 = CONV #( wa_cabecalho-lc_coleta_parid )
                              attr2 = CONV #( lc_a_lzone )
                              attr3 = CONV #( wa_cabecalho-lc_entrega_parid )
                              attr4 = CONV #( lc_l_lzone )  )
            msgty  = 'E'
            msgno  = zcx_delivery=>zcx_sem_itinerario-msgno
            msgv1  = CONV #( wa_cabecalho-lc_coleta_parid )
            msgv2  = CONV #( lc_a_lzone )
            msgv3  = CONV #( wa_cabecalho-lc_entrega_parid )
            msgv4  = CONV #( lc_l_lzone )
            msgid  = zcx_delivery=>zcx_sem_itinerario-msgid.
      ENDIF.
    ENDIF.

    "Fornecedor da Mercadoria
    IF wa_cabecalho-lifnr IS NOT INITIAL.

      CLEAR: wa_gn_partner.
      wa_gn_partner-rfbel = lc_delivery.

      CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
        EXPORTING
          input  = 'LF'
        IMPORTING
          output = wa_gn_partner-parvw.

      wa_gn_partner-lifnr = wa_cabecalho-lifnr.
      APPEND wa_gn_partner TO it_gn_partner.

    ENDIF.

    "Local de Coleta
    IF wa_cabecalho-lc_coleta_parid IS NOT INITIAL.

      CLEAR: wa_gn_partner.
      wa_gn_partner-rfbel = lc_delivery.
      CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
        EXPORTING
          input  = 'PC'
        IMPORTING
          output = wa_gn_partner-parvw.
      wa_gn_partner-lifnr = wa_cabecalho-lc_coleta_parid.
      APPEND wa_gn_partner TO it_gn_partner.

    ENDIF.

    "Local de Entrega
    IF wa_cabecalho-lc_entrega_parid IS NOT INITIAL.

      CLEAR: wa_gn_partner.
      wa_gn_partner-rfbel = lc_delivery.
      CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
        EXPORTING
          input  = 'LR'
        IMPORTING
          output = wa_gn_partner-parvw.
      wa_gn_partner-kunnr = wa_cabecalho-lc_entrega_parid.
      APPEND wa_gn_partner TO it_gn_partner.

    ENDIF.

    "Agente de Frete
    IF wa_cabecalho-sp_frete_parid IS NOT INITIAL.
      CLEAR: wa_gn_partner.
      wa_gn_partner-rfbel = lc_delivery.
      wa_gn_partner-parvw = 'SP'.
      wa_gn_partner-lifnr = wa_cabecalho-sp_frete_parid.
      APPEND wa_gn_partner TO it_gn_partner.
    ENDIF.

    "Fornecedor Mercadoria
    IF wa_cabecalho-wl_forn_merc_parid IS NOT INITIAL.
      CLEAR: wa_gn_partner.
      wa_gn_partner-rfbel = lc_delivery.
      CALL FUNCTION 'CONVERSION_EXIT_PARVW_INPUT'
        EXPORTING
          input  = 'WL'
        IMPORTING
          output = wa_gn_partner-parvw.
      wa_gn_partner-lifnr = wa_cabecalho-wl_forn_merc_parid.
      APPEND wa_gn_partner TO it_gn_partner.
    ENDIF.

    lc_item_number = 10.

    CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
      IMPORTING
        own_logical_system             = lc_sender_system
      EXCEPTIONS
        own_logical_system_not_defined = 1
        OTHERS                         = 2.

    "ITEMS_PARTICIONAR = ME->ITEMS.

    "    IF ME->PARTICAO_LOTE EQ ABAP_TRUE.

    LOOP AT me->items INTO DATA(wa_itm).
      IF wa_itm-item_do_lote IS INITIAL.
        APPEND wa_itm TO items_particionar.
      ELSE.
        READ TABLE items_particionar WITH KEY item_do_lote = wa_itm-item_do_lote TRANSPORTING NO FIELDS.
        IF sy-subrc IS NOT INITIAL.
          APPEND wa_itm TO items_particionar.
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT items_particionar INTO DATA(wa_itens).

      SELECT SINGLE *
        FROM ekko
        INTO @DATA(_ekko)
       WHERE ebeln = @wa_itens-ebeln.

      SELECT SINGLE *
        FROM ekpv
        INTO @DATA(_ekpv)
       WHERE ebeln = @wa_itens-ebeln
         AND ebelp = @wa_itens-ebelp.

      SELECT SINGLE *
        FROM ekpo
        INTO @DATA(_ekpo)
       WHERE ebeln = @wa_itens-ebeln
         AND ebelp = @wa_itens-ebelp.

      SELECT SINGLE *
        FROM tvlk
        INTO @DATA(_tvlk)
       WHERE lfart = @_ekpo-lfret.

      CLEAR wa_xkomdlgn.

      MOVE-CORRESPONDING _ekpv TO wa_xkomdlgn.

*-------------------------------------------------------------------
      ls_komdlgn-kunwe = _ekpv-kunnr.

      IF _ekpo-fls_rsto = abap_true.                      "v_n_2073218
        CALL FUNCTION 'VIEW_KNA1'
          EXPORTING
            kunde     = ls_komdlgn-kunwe
          IMPORTING
            anschrift = ls_kna1
          EXCEPTIONS
            no_kna1   = 1
            OTHERS    = 2.
        IF NOT ls_kna1 IS INITIAL.
          ls_komdlgn-adrnr_we = ls_kna1-adrnr.
          ls_komdlgn-adrda_we = 'D'.
        ENDIF.                                                "^_n_2073218
      ELSEIF NOT _ekpo-adrnr IS INITIAL.             "Anlieferadresse
        ls_komdlgn-adrnr_we = _ekpo-adrnr.
        ls_komdlgn-adrda_we = 'E'.
      ELSEIF NOT _ekpo-adrn2 IS INITIAL.
        ls_komdlgn-adrnr_we = _ekpo-adrn2.
        ls_komdlgn-adrda_we = 'D'.
      ELSEIF NOT _ekpo-emlif IS INITIAL.
        CALL FUNCTION 'WY_LFA1_SINGLE_READ'
          EXPORTING
            pi_lifnr         = _ekpo-emlif
          IMPORTING
            po_lfa1          = ls_lfa1
          EXCEPTIONS
            no_records_found = 1
            OTHERS           = 2.
        IF sy-subrc = 0 AND NOT ls_lfa1-adrnr IS INITIAL.
          ls_komdlgn-adrnr_we = ls_lfa1-adrnr.
          ls_komdlgn-adrda_we = 'D'.                      "50A
        ENDIF.
      ELSEIF NOT _ekpo-kunnr IS INITIAL.
        CALL FUNCTION 'VIEW_KNA1'
          EXPORTING
            kunde     = _ekpo-kunnr
          IMPORTING
            anschrift = ls_kna1
          EXCEPTIONS
            no_kna1   = 1
            OTHERS    = 2.

        IF NOT ls_kna1 IS INITIAL.
          ls_komdlgn-adrnr_we = ls_kna1-adrnr.
          ls_komdlgn-adrda_we = 'D'.                      "50A
        ENDIF.
      ELSE.                                                 "v_n_657200
        CALL FUNCTION 'VIEW_KNA1'
          EXPORTING
            kunde     = ls_komdlgn-kunwe
          IMPORTING
            anschrift = ls_kna1
          EXCEPTIONS
            no_kna1   = 1
            OTHERS    = 2.
        IF NOT ls_kna1 IS INITIAL.
          ls_komdlgn-adrnr_we = ls_kna1-adrnr.
          ls_komdlgn-adrda_we = 'D'.
        ENDIF.                                              "^_n_657200
      ENDIF.
*-------------------------------------------------------------------

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lc_item_number
        IMPORTING
          output = lc_item_number.

      wa_xkomdlgn-rfbel    = _ekko-ebeln.
      wa_xkomdlgn-rfpos    = _ekpo-ebelp.
      wa_xkomdlgn-mandt    = sy-mandt.
      wa_xkomdlgn-vstel    = _ekpv-vstel.
      wa_xkomdlgn-vkorg    = _ekpv-vkorg.
      wa_xkomdlgn-bukrs_best = _ekpv-vkorg.
      wa_xkomdlgn-vtweg    = _ekpv-vtweg.
      wa_xkomdlgn-kunwe    = ls_komdlgn-kunwe.
      wa_xkomdlgn-adrnr_we = ls_komdlgn-adrnr_we.
      wa_xkomdlgn-adrda_we = ls_komdlgn-adrda_we.
      wa_xkomdlgn-matkl    = _ekpo-matkl.
      wa_xkomdlgn-spart    = _ekpv-spart.
      wa_xkomdlgn-vsbed    = _ekpv-spart.
      wa_xkomdlgn-lfart    = _ekpo-lfret.
      wa_xkomdlgn-auart    = _tvlk-daart.
      wa_xkomdlgn-ewerk    = _ekpo-werks.
      wa_xkomdlgn-spe_currency = _ekko-waers.
*     wa_xkomdlgn-dlvtp    = 'J'.     "'7'.
      wa_xkomdlgn-matnr    = wa_itens-material.
      wa_xkomdlgn-werks    = _ekpv-vstel.
      wa_xkomdlgn-lfdat    = sy-datlo.
      wa_xkomdlgn-lfuhr    = sy-timlo.
      wa_xkomdlgn-umvkz    = 1.
      wa_xkomdlgn-umvkn    = 1.
      wa_xkomdlgn-vrkme    = wa_itens-unidade.
      wa_xkomdlgn-meins    = wa_itens-unidade.
      wa_xkomdlgn-vgbel    = wa_itens-ebeln.
      wa_xkomdlgn-vgpos    = wa_itens-ebelp.
      wa_xkomdlgn-vgtyp    = wa_itens-vgtyp.
      wa_xkomdlgn-vwpos    = wa_itens-vgtyp.
      wa_xkomdlgn-verursys = lc_sender_system.
      wa_xkomdlgn-kunag    = wa_cabecalho-sp_frete_parid.
      wa_xkomdlgn-vfdat    = wa_cabecalho-vfdat.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_cabecalho-sp_frete_parid
        IMPORTING
          output = wa_cabecalho-sp_frete_parid.

      DATA(str_qtd) = strlen( wa_cabecalho-sp_frete_parid ).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_cabecalho-sp_frete_parid
        IMPORTING
          output = wa_cabecalho-sp_frete_parid.

      IF str_qtd LE 4.

        DATA: lc_j_1bbranch TYPE j_1bbranch.
        MOVE wa_cabecalho-sp_frete_parid TO lc_j_1bbranch-branch.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lc_j_1bbranch-branch
          IMPORTING
            output = lc_j_1bbranch-branch.

        SELECT SINGLE * INTO lc_j_1bbranch FROM j_1bbranch
          WHERE branch = lc_j_1bbranch-branch .

        IF sy-subrc IS INITIAL.
          wa_xkomdlgn-inco1   = 'CIF'.
          wa_xkomdlgn-inco2   = 'CIF'.
        ELSE.
          wa_xkomdlgn-inco1   = 'CPT'.
          wa_xkomdlgn-inco2   = 'CPT'.
        ENDIF.

      ELSE.
        wa_xkomdlgn-inco1   = 'CPT'.
        wa_xkomdlgn-inco2   = 'CPT'.
      ENDIF.

      wa_xkomdlgn-lgort    = wa_itens-stge_loc.
      wa_xkomdlgn-gewei    = wa_itens-unidade.
      wa_xkomdlgn-lifnr    = wa_cabecalho-lifnr.
      wa_xkomdlgn-traty    = wa_itens-traty.
      wa_xkomdlgn-tragr    = wa_itens-tragr.
      wa_xkomdlgn-ladgr    = wa_itens-ladgr.
      wa_xkomdlgn-mfrgr    = wa_itens-mfrgr.

*     vl_lfimg = wa_cabecalho-vl_total_fatura.
*     SHIFT vl_lfimg LEFT DELETING LEADING space.
*     TRANSLATE vl_lfimg USING '.,'.
*     wa_xkomdlgn-bolnr    = vl_lfimg.

      wa_xkomdlgn-kzbew    = wa_itens-kzbew.
      wa_xkomdlgn-noatp    = abap_true.
      wa_xkomdlgn-xchar    = abap_true.
      wa_xkomdlgn-xchpf    = abap_true.
      wa_xkomdlgn-lifex    = wa_cabecalho-xblnr.
      wa_xkomdlgn-route    = wa_cabecalho-route.

      "Verificar se Pedido de Compra é para Movimentar Mercadoria
      TRY .
          DATA(r_bstae) = zcl_pedido_compra=>get_chave_controle_conf_item( EXPORTING i_ebeln = wa_itens-ebeln i_ebelp = wa_itens-ebelp ).
          SELECT SINGLE * INTO @DATA(wa_t163g)
            FROM t163g
           WHERE bstae EQ @r_bstae
             AND blfdn EQ '1'
             AND ebtyp EQ 'LA'.

          IF ( sy-subrc IS INITIAL ) AND ( wa_t163g-werel EQ abap_true OR wa_t163g-wezuo EQ abap_true ).
            wa_xkomdlgn-nowab = abap_false.
          ELSEIF sy-subrc IS INITIAL.
            wa_xkomdlgn-nowab = abap_true.
          ENDIF.

        CATCH zcx_pedido_compra_exception.
      ENDTRY.

      SELECT SINGLE maktx INTO wa_xkomdlgn-arktx
        FROM makt
       WHERE spras EQ sy-langu
         AND matnr EQ wa_itens-material.

      IF ( me->particao_lote EQ abap_false ) OR ( wa_itens-item_do_lote IS INITIAL ).
        wa_xkomdlgn-rfpos    = lc_item_number.
        wa_xkomdlgn-charg    = wa_itens-batch.
        wa_xkomdlgn-lichn    = wa_itens-licha.
        wa_xkomdlgn-lfimg    = wa_itens-quantidade.
        wa_xkomdlgn-lgmng    = wa_itens-quantidade.
        wa_xkomdlgn-ntgew    = wa_itens-quantidade.
        wa_xkomdlgn-brgew    = wa_itens-quantidade.
        wa_xkomdlgn-noatp    = abap_false.
        APPEND wa_xkomdlgn TO it_xkomdlgn.
      ELSE.
        wa_xkomdlgn-rfpos    = lc_item_number.
        wa_xkomdlgn-charg    = ''.
        wa_xkomdlgn-lichn    = ''.
        wa_xkomdlgn-lfimg    = 0.
        wa_xkomdlgn-lgmng    = 0.
        wa_xkomdlgn-ntgew    = 0.
        wa_xkomdlgn-brgew    = 0.
        wa_xkomdlgn-kzazu    = 'X'.
*       wa_xkomdlgn-vsbed    = '01'.
*       wa_xkomdlgn-traty    = ''.
        wa_xkomdlgn-noatp    = abap_false.
*       wa_xkomdlgn-posar    = 'B'.
        wa_xkomdlgn-kcgewei  = wa_itens-unidade.
        TRANSLATE wa_itens-unidade TO UPPER CASE.
        IF  wa_itens-unidade = 'L' OR   wa_itens-unidade = 'BAG'.
          wa_xkomdlgn-kcgewei    = 'KG'.
        ENDIF.
*        wa_xkomdlgn-ematn    = wa_itens-material.
*        WA_XKOMDLGN-KCGEWEI  = WA_ITENS-UNIDADE.
        APPEND wa_xkomdlgn TO it_xkomdlgn.

        READ TABLE it_xkomdlgn ASSIGNING FIELD-SYMBOL(<fs_item>) WITH KEY rfpos = lc_item_number.
        SELECT SINGLE *
          FROM marm
          INTO @DATA(wmarm)
        WHERE matnr = @wa_itens-material
        AND   meinh = @wa_itens-unidade
        AND   gewei = @wa_xkomdlgn-kcgewei.

        LOOP AT me->items INTO DATA(wa_lote) WHERE item_do_lote EQ wa_itens-item_do_lote.
*          SELECT SINGLE *
*            INTO @DATA(wzib_nfe_dist_lot)
*            FROM zib_nfe_dist_lot
*            WHERE chave_nfe = @wa_cabecalho-chave_nfe
*            AND   matnr     = @wa_itens-material
*            AND   charg     = @wa_lote-batch.
*          IF sy-subrc = 0.
*            SELECT SINGLE *
*              FROM zib_nfe_dist_lca
*              INTO @DATA(wzib_nfe_dist_lca)
*              WHERE cd_lote_item = @wzib_nfe_dist_lot-cd_lote_item
*              AND   atnam        = 'PESO_BAG'.
*            IF sy-subrc = 0.
*              REPLACE ',' IN wzib_nfe_dist_lca-atwrt WITH  '.'.
*              wmarm-brgew = wzib_nfe_dist_lca-atwrt.
*            ENDIF.
*          ENDIF.

          ADD wa_lote-quantidade TO <fs_item>-kcmeng.
          vl_kcmeng = wa_lote-quantidade.
          IF wmarm-brgew GT 0.
            vl_kcmeng = ( wa_lote-quantidade * wmarm-brgew ).
            ADD vl_kcmeng TO <fs_item>-kcbrgew.
            ADD vl_kcmeng TO <fs_item>-kcntgew.
            vl_kcmeng =  wmarm-brgew.
          ELSE.
            ADD wa_lote-quantidade TO <fs_item>-kcbrgew.
            ADD wa_lote-quantidade TO <fs_item>-kcntgew..
          ENDIF.

          wa_xkomdlgn-rfpos = 0.
          wa_xkomdlgn-uecha = wa_xkomdlgn-rfpos.
          wa_xkomdlgn-charg = wa_lote-batch.
          wa_xkomdlgn-lichn = wa_lote-licha.
          wa_xkomdlgn-lfimg = wa_lote-quantidade.
          wa_xkomdlgn-lgmng = wa_lote-quantidade.
          IF wa_itens-unidade = 'BAG'.
            wa_xkomdlgn-gewei = 'KG'.
          ENDIF.
          wa_xkomdlgn-ntgew = wa_lote-quantidade. "vl_kcmeng.
          wa_xkomdlgn-brgew = wa_lote-quantidade. "vl_kcmeng.
          wa_xkomdlgn-noatp = abap_false.
          APPEND wa_xkomdlgn TO it_xkomdlgn.
        ENDLOOP.
      ENDIF.

      ADD 10 TO lc_item_number.
    ENDLOOP.

    CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
      IMPORTING
        own_logical_system             = lc_sender_system
      EXCEPTIONS
        own_logical_system_not_defined = 1
        OTHERS                         = 2.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_delivery
        EXPORTING
          textid = VALUE #( msgid = sy-msgid
                            msgno = sy-msgno
                            attr1 = CONV #( sy-msgv1 )
                            attr2 = CONV #( sy-msgv2 )
                            attr3 = CONV #( sy-msgv3 )
                            attr4 = CONV #( sy-msgv4 ) )
          msgty  = 'E'
          msgno  = sy-msgno
          msgid  = sy-msgid
          msgv1  = sy-msgv1
          msgv2  = sy-msgv2
          msgv3  = sy-msgv3
          msgv4  = sy-msgv4.
    ENDIF.

*-----------------------------------------------
* --criar remessa
*-----------------------------------------------
    CALL FUNCTION 'GN_DELIVERY_CREATE'
      EXPORTING
        vbsk_i        = i_vbsk
      TABLES
        xkomdlgn      = it_xkomdlgn
        xvbfs         = it_xvbfs
        xvbls         = it_xvbls
        it_gn_partner = it_gn_partner.

    IF sy-subrc IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      "Forçar a liberação do documento gerado
      CALL FUNCTION 'DEQUEUE_ALL'
        EXPORTING
          _synchron = 'X'.

      DELETE it_xvbls WHERE vbeln_lif IS INITIAL.
      SORT it_xvbls BY vbeln_lif.
      DELETE ADJACENT DUPLICATES FROM it_xvbls COMPARING vbeln_lif.
      DESCRIBE TABLE it_xvbls LINES DATA(qtd_linhas).

      IF qtd_linhas GE 1.
        r_gerou = abap_true.
        READ TABLE it_xvbls INDEX 1 INTO DATA(wa_xvbls).
        me->set_nr_remessa( i_remessa = wa_xvbls-vbeln_lif ).
        COMMIT WORK.
      ELSE.
        r_gerou = abap_false.
      ENDIF.
    ELSE.
      r_gerou = abap_false.
    ENDIF.

*-------------------------------------------------
*-- retorno erro
*-------------------------------------------------
    IF r_gerou = abap_false.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      LOOP AT it_xvbfs INTO DATA(wa_xvbfs).
        MESSAGE ID     wa_xvbfs-msgid
                TYPE   wa_xvbfs-msgty
                NUMBER wa_xvbfs-msgno
                WITH   wa_xvbfs-msgv1 wa_xvbfs-msgv2 wa_xvbfs-msgv3 wa_xvbfs-msgv4
                INTO   wa_retorno-message.

        wa_retorno-type        = sy-msgty.
        wa_retorno-id          = sy-msgid.
        wa_retorno-number      = sy-msgno.
        wa_retorno-message_v1  = sy-msgv1.
        wa_retorno-message_v2  = sy-msgv2.
        wa_retorno-message_v3  = sy-msgv3.
        wa_retorno-message_v4  = sy-msgv4.
        APPEND wa_retorno TO lc_retorno.
      ENDLOOP.

      me->set_retorno( i_retorno = lc_retorno[] ).

      LOOP AT items_particionar INTO wa_itens.
        zcl_frete_remessa_trans=>zif_frete_remessa_trans~set_criar_log( EXPORTING i_vbeln      = me->av_vbeln
                                                                                  i_ebeln      = CONV #( wa_itens-ebeln )
                                                                                  i_ebelp      = CONV #( wa_itens-ebelp )
                                                                                  i_etapa_proc = '02' "C_ETAPA_CRIAR_REMESSA
                                                                                  i_commit     = abap_true
                                                                        CHANGING  t_return     = me->retorno[] ).
      ENDLOOP.
      RETURN.
    ENDIF.

*-------------------------------------------------
*-- ajustar remessa
*-------------------------------------------------
    LOOP AT me->items INTO wa_itm.
      lc_quantidade = lc_quantidade + wa_itm-quantidade.
    ENDLOOP.

    LOOP AT items_particionar INTO wa_itens.
      wa_dados_processa_po-ebeln       = wa_itens-ebeln.
      wa_dados_processa_po-ebelp       = wa_itens-ebelp.
      wa_dados_processa_po-menge       = lc_quantidade.
      wa_dados_processa_po-meins       = wa_itens-unidade.
      wa_dados_processa_po-matnr       = wa_itens-material.
      wa_dados_processa_po-ped_werks   = wa_cabecalho-ship_point.
      wa_dados_processa_po-ped_entrega = wa_cabecalho-lc_entrega_parid.
      wa_dados_processa_po-lgort       = wa_itens-stge_loc.
*     wa_dados_processa_po-charg       = wa_itens-batch.
      wa_dados_processa_po-ped_coleta  = wa_cabecalho-lc_coleta_parid.
      APPEND wa_dados_processa_po     TO gt_dados_processa_po.
    ENDLOOP.

    w_header_data-deliv_numb     = me->av_vbeln.
    w_header_control-deliv_numb  = 'X'.

    w_header_partner-upd_mode_partn = 'I'.
    w_header_partner-deliv_numb     = me->av_vbeln.
    w_header_partner-itm_number     = '000010'.
    w_header_partner-partn_role     = 'PC'.
    w_header_partner-partner_no     = wa_dados_processa_po-ped_coleta.
    APPEND w_header_partner TO t_header_partner.

    w_header_partner-upd_mode_partn = 'I'.
    w_header_partner-deliv_numb     = me->av_vbeln.
    w_header_partner-itm_number     = '000010'.
    w_header_partner-partn_role     = 'LR'.
    w_header_partner-partner_no     = wa_dados_processa_po-ped_entrega.
    APPEND w_header_partner TO t_header_partner.

    LOOP AT gt_dados_processa_po INTO wa_dados_processa_po.
      w_item_data-deliv_numb          = me->av_vbeln.

      SELECT SINGLE posnr
        FROM lips
        INTO  w_item_data-deliv_item
        WHERE vbeln = me->av_vbeln
        AND   vgbel = wa_dados_processa_po-ebeln
        AND   vgpos = wa_dados_processa_po-ebelp.

*     w_item_data-batch               = wa_dados_processa_po-charg.
*     w_item_data-dlv_qty             = wa_dados_processa_po-menge.
*     w_item_data-dlv_qty_imunit      = wa_dados_processa_po-menge.
      w_item_data-fact_unit_nom       = 1.
      w_item_data-fact_unit_denom     = 1.
*     w_item_data-gross_wt            = wa_dados_processa_po-menge.
*     w_item_data-net_weight          = wa_dados_processa_po-menge.
      APPEND w_item_data TO t_item_data.
      "
      w_item_control-deliv_numb       = me->av_vbeln.
      w_item_control-deliv_item       = w_item_data-deliv_item.
*     w_item_control-chg_delqty       = 'X'.
*     w_item_control-volume_flg       = 'X'.
*     w_item_control-net_wt_flg       = 'X'.
*     w_item_control-gross_wt_flg     = 'X'.
      APPEND w_item_control TO t_item_control.
      "
      w_item_data_spl-deliv_numb      = me->av_vbeln.
      w_item_data_spl-deliv_item      = w_item_data-deliv_item.
*     w_item_data_spl-stge_loc        = wa_dados_processa_po-lgort.
*     APPEND w_item_data_spl TO t_item_data_spl.
    ENDLOOP.

    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
      EXPORTING
        header_data    = w_header_data
        header_control = w_header_control
        delivery       = me->av_vbeln
      TABLES
        header_partner = t_header_partner
        item_data      = t_item_data
        item_control   = t_item_control
        return         = t_retorno2
        item_data_spl  = t_item_data_spl.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

*-------------------------------------------------
*-- picking
*-------------------------------------------------
    DATA(lc_gerou_piking) = me->set_piking( items_particionar ).

    IF lc_gerou_piking = abap_true.
      CALL FUNCTION 'DEQUEUE_ALL'
        EXPORTING
          _synchron = 'X'.

      LOOP AT gt_dados_processa_po INTO wa_dados_processa_po.
        wa_dados_remessa-rem_vbeln    =  me->av_vbeln.
        wa_dados_remessa-ped_ebeln    =  wa_dados_processa_po-ebeln.
        wa_dados_remessa-rem_brgew    =  wa_dados_processa_po-menge.
        APPEND wa_dados_remessa      TO e_dados_remessa.
      ENDLOOP.
    ELSE.
      me->set_elimina_remessa( me->av_vbeln ).
    ENDIF.

  ENDMETHOD.


  METHOD set_criar_remessa.

    TYPES: BEGIN OF ty_dados_pedidos,
             ebeln       TYPE ekpo-ebeln,
             ebelp       TYPE ekpo-ebelp,
             menge       TYPE ekpo-menge,
             meins       TYPE ekpo-meins,
             matnr       TYPE ekpo-matnr,
             ped_werks   TYPE ekpo-werks,
             ped_entrega TYPE lfa1-lifnr,
             lgort       TYPE ekpo-lgort,
             charg       TYPE eket-charg,
             ped_coleta  TYPE ekpa-lifn2,
           END   OF ty_dados_pedidos.

    TYPES: BEGIN OF ty_dados_processa_po,
             ebeln       TYPE ekpo-ebeln,
             ebelp       TYPE ekpo-ebelp,
             menge       TYPE ekpo-menge,
             meins       TYPE ekpo-meins,
             matnr       TYPE ekpo-matnr,
             ped_werks   TYPE ekpo-werks,
             ped_entrega TYPE lfa1-lifnr,
             lgort       TYPE ekpo-lgort,
             charg       TYPE eket-charg,
             ped_coleta  TYPE ekpa-lifn2,
           END   OF ty_dados_processa_po.

    DATA: nlinhas              TYPE i,
          v_due_date           TYPE ledat,
          v_deliv_numb         TYPE bapishpdelivnumb-deliv_numb,
          v_erro               TYPE c LENGTH 1,
          v_vgpos              TYPE lips-vgpos,
          v_tabix              TYPE sy-tabix,
          v_route1             TYPE ekpv-route,
          v_route2             TYPE ekpv-route,
          w_ekpo1              TYPE ekpo,
          w_ekpo2              TYPE ekpo,
          w_eket1              TYPE eket,
          w_eket2              TYPE eket,
          t_lotes              TYPE zib_nfe_dist_lot_t,
          t_lotes_aux          TYPE zib_nfe_dist_lot_t,
          t_items              TYPE TABLE OF bapishpdelivnumb,
          t_itens              TYPE TABLE OF bapidlvreftosto,
          w_itens              TYPE bapidlvreftosto,
          t_retorno            TYPE TABLE OF bapiret2,
          lc_lotes             TYPE zib_nfe_dist_lot_t,
          lc_quantidade        TYPE lfimg,
          vl_tot_menge         TYPE ekpo-menge,
          lc_chave_nfe         TYPE zib_nfe_dist_itm-chave_nfe,
          gt_dados_pedidos     TYPE TABLE OF ty_dados_pedidos,
          w_header_data        TYPE bapiobdlvhdrchg,
          wa_dados_remessa     TYPE zzsd_dados_remessa,
          w_header_control     TYPE bapiobdlvhdrctrlchg,
          t_header_partner     TYPE TABLE OF bapidlvpartnerchg,
          w_header_partner     TYPE bapidlvpartnerchg,
          t_item_data          TYPE TABLE OF bapiobdlvitemchg,
          t_item_control       TYPE TABLE OF bapiobdlvitemctrlchg,
          t_item_data_spl      TYPE TABLE OF /spe/bapiobdlvitemchg,
          w_item_data          TYPE bapiobdlvitemchg,
          w_item_control       TYPE bapiobdlvitemctrlchg,
          w_item_data_spl      TYPE /spe/bapiobdlvitemchg,
          t_retorno2           TYPE TABLE OF bapiret2,
          wa_dados_po          TYPE ty_dados_pedidos,
          wa_dados_pedidos     TYPE ty_dados_pedidos,
          wa_log_remessa       TYPE zlest0247,
          items_particionar	   TYPE zde_bapi_remessa_item_t,
          gt_dados_processa_po TYPE TABLE OF ty_dados_processa_po,
          wa_dados_processa_po TYPE ty_dados_processa_po.

    MOVE-CORRESPONDING: t_itens_ped[] TO gt_dados_pedidos[],
                        t_itens_ped[] TO items_particionar[],
                        t_lotes_ped[] TO t_lotes[].

    DELETE gt_dados_pedidos  WHERE ebeln IS INITIAL.
    DELETE items_particionar WHERE ebeln IS INITIAL.
    DELETE t_lotes           WHERE chave_nfe IS INITIAL.

    FREE: e_log_remessa.

    LOOP AT gt_dados_pedidos INTO wa_dados_pedidos WHERE NOT ebeln IS INITIAL.
      v_tabix = sy-tabix.

      SELECT SINGLE ebeln, ebelp, matnr, werks, lgort, menge, meins
             FROM ekpo
             INTO @DATA(wa_ekpo)
             WHERE ebeln = @wa_dados_pedidos-ebeln
             AND   ebelp = @wa_dados_pedidos-ebelp.

      SELECT ebeln, ebelp, lifn2 UP TO 1 ROWS
             FROM ekpa
             INTO @DATA(wa_ekpa)
             WHERE ebeln = @wa_ekpo-ebeln
             AND   parvw  = 'PR'.
      ENDSELECT.

      SELECT ebeln, ebelp, charg UP TO 1 ROWS
             FROM eket
             INTO @DATA(wa_eket)
             WHERE ebeln = @wa_ekpo-ebeln
             AND  ebelp  = @wa_ekpo-ebelp.
      ENDSELECT.

      wa_dados_pedidos-ebeln     = wa_ekpo-ebeln.
      wa_dados_pedidos-ebelp     = wa_ekpo-ebelp.
*     wa_dados_pedidos-menge     = wa_ekpo-menge.
      wa_dados_pedidos-meins     = wa_ekpo-meins.
      wa_dados_pedidos-matnr     = wa_ekpo-matnr.
      wa_dados_pedidos-ped_werks = wa_ekpo-werks.
      wa_dados_pedidos-lgort     = wa_ekpo-lgort.

      "    Transformar XPED-RESWR é um código de fornecedor
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_ekpo-werks
        IMPORTING
          output = wa_dados_pedidos-ped_entrega.

      wa_dados_pedidos-charg      = wa_eket-charg.
      wa_dados_pedidos-ped_coleta = wa_ekpa-lifn2.
      MODIFY gt_dados_pedidos FROM wa_dados_pedidos INDEX v_tabix.
    ENDLOOP.

*---------------------------------------
*-- ajustar tabela T_LOTES
*---------------------------------------
    LOOP AT gt_dados_pedidos INTO wa_dados_pedidos.
      lc_chave_nfe = wa_dados_pedidos-ebeln && wa_dados_pedidos-ebelp.
      READ TABLE t_lotes INTO DATA(ws_lotes) WITH KEY chave_nfe = lc_chave_nfe.
      IF sy-subrc <> 0.
        ws_lotes-chave_nfe  = lc_chave_nfe.
        ws_lotes-charg      = wa_dados_pedidos-charg.
*       ws_lotes-charg      = COND #( WHEN wa_dados_pedidos-charg IS INITIAL THEN sy-datum(4)
*                                                                            ELSE wa_dados_pedidos-charg ).
        APPEND ws_lotes    TO t_lotes.
      ENDIF.
    ENDLOOP.

*---------------------------------------
*-- area BAPI change
*---------------------------------------
    SORT gt_dados_pedidos BY ebeln ebelp.

    LOOP AT gt_dados_pedidos        INTO wa_dados_pedidos WHERE NOT ebeln IS INITIAL.
      CLEAR w_itens.
      w_itens-ref_doc                  = wa_dados_pedidos-ebeln.
      w_itens-ref_item                 = wa_dados_pedidos-ebelp.
      w_itens-dlv_qty                  = wa_dados_pedidos-menge.
      w_itens-sales_unit               = wa_dados_pedidos-meins.
      w_itens-sales_unit_iso           = wa_dados_pedidos-meins .
      APPEND w_itens                  TO t_itens.

      wa_dados_processa_po-ebeln       = wa_dados_pedidos-ebeln.
      wa_dados_processa_po-ebelp       = wa_dados_pedidos-ebelp.
      wa_dados_processa_po-menge       = wa_dados_pedidos-menge.
      wa_dados_processa_po-meins       = wa_dados_pedidos-meins.
      wa_dados_processa_po-matnr       = wa_dados_pedidos-matnr.
      wa_dados_processa_po-ped_werks   = wa_dados_pedidos-ped_werks.
      wa_dados_processa_po-ped_entrega = wa_dados_pedidos-ped_entrega.
      wa_dados_processa_po-lgort       = wa_dados_pedidos-lgort.
      wa_dados_processa_po-charg       = wa_dados_pedidos-charg.
      wa_dados_processa_po-ped_coleta  = wa_dados_pedidos-ped_coleta.
      APPEND wa_dados_processa_po     TO gt_dados_processa_po.
      wa_dados_po                      = wa_dados_pedidos.
    ENDLOOP.

    CHECK t_itens[] IS NOT INITIAL.

*---------------------------------------
*-- exportar LOTES para funcao GN_DELIVERY_CREATE
*---------------------------------------
    FREE MEMORY ID 'PARTICAO_LOTES'.

    EXPORT t_lotes FROM t_lotes[] TO MEMORY ID 'PARTICAO_LOTES'.

*---------------------------------------
*-- criar remessa
*---------------------------------------
    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_STO'  "#EC CI_USAGE_OK[2438131]
      EXPORTING
        due_date          = v_due_date
      IMPORTING
        delivery          = v_deliv_numb
      TABLES
        stock_trans_items = t_itens
        return            = t_retorno
        deliveries        = t_items.

    FREE MEMORY ID 'PARTICAO_LOTES'.

    READ TABLE t_retorno INTO DATA(w_retorno) WITH KEY type = 'S'.

    IF sy-subrc IS INITIAL AND v_deliv_numb IS NOT INITIAL.
      r_gerou = abap_true.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      "Forçar a liberação do documento gerado
      CALL FUNCTION 'DEQUEUE_ALL'
        EXPORTING
          _synchron = 'X'.

      me->set_nr_remessa( i_remessa = v_deliv_numb ).
    ELSE.
*---- retorno erro ----------------------------------------------
      r_gerou = abap_false.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      me->set_retorno( i_retorno = t_retorno[] ).

      LOOP AT t_retorno INTO DATA(wa_retorno).
        CLEAR wa_log_remessa.
        wa_log_remessa-ebeln    = wa_dados_po-ebeln.
        wa_log_remessa-tipo_msg = wa_retorno-type.
        wa_log_remessa-number   = wa_retorno-number.

        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = wa_retorno-id
            msgnr               = wa_retorno-number
            msgv1               = wa_retorno-message_v1
            msgv2               = wa_retorno-message_v2
            msgv3               = wa_retorno-message_v3
            msgv4               = wa_retorno-message_v4
          IMPORTING
            message_text_output = wa_log_remessa-message.

        APPEND wa_log_remessa TO e_log_remessa.
      ENDLOOP.
      RETURN.
    ENDIF.

*-------------------------------------------------
*-- ajustar remessa
*-------------------------------------------------
    LOOP AT t_items INTO DATA(w_items).

      FREE: w_header_data, w_header_control, t_retorno2[], t_header_partner[], t_item_data[], t_item_control[], t_item_data_spl[].

      SELECT SINGLE vgbel, vgpos
        FROM lips
        INTO @DATA(w_lips)
       WHERE vbeln = @w_items-deliv_numb.

      me->set_nr_remessa( i_remessa = w_items-deliv_numb ).

      LOOP AT gt_dados_processa_po INTO wa_dados_processa_po WHERE ebeln = w_lips-vgbel
                                                               AND ebelp = w_lips-vgpos+1(5).

        w_header_data-deliv_numb        = me->av_vbeln.
        w_header_control-deliv_numb     = 'X'.

        w_header_partner-upd_mode_partn = 'I'.
        w_header_partner-deliv_numb     = me->av_vbeln.
        w_header_partner-itm_number     = '000010'.
        w_header_partner-partn_role     = 'PC'.
        w_header_partner-partner_no     = wa_dados_processa_po-ped_coleta.
        APPEND w_header_partner        TO t_header_partner.

        w_header_partner-upd_mode_partn = 'I'.
        w_header_partner-deliv_numb     = me->av_vbeln.
        w_header_partner-itm_number     = '000010'.
        w_header_partner-partn_role     = 'LR'.
        w_header_partner-partner_no     = wa_dados_processa_po-ped_entrega.
        APPEND w_header_partner        TO t_header_partner.

        w_item_data-deliv_numb        = me->av_vbeln.

        SELECT SINGLE posnr
          FROM lips
          INTO w_item_data-deliv_item
         WHERE vbeln = me->av_vbeln
           AND vgbel = wa_dados_processa_po-ebeln
           AND vgpos = wa_dados_processa_po-ebelp.

        w_item_data-batch               = wa_dados_processa_po-charg.
*       w_item_data-batch               = COND #( WHEN wa_dados_processa_po-charg IS INITIAL THEN sy-datum(4)
*                                                                                            ELSE wa_dados_processa_po-charg ).
*       w_item_data-dlv_qty             = wa_dados_processa_po-menge.
*       w_item_data-dlv_qty_imunit      = wa_dados_processa_po-menge.
        w_item_data-fact_unit_nom       = 1.
        w_item_data-fact_unit_denom     = 1.
*       w_item_data-gross_wt            = wa_dados_processa_po-menge.
*       w_item_data-net_weight          = wa_dados_processa_po-menge.
        APPEND w_item_data TO t_item_data.
        "
        w_item_control-deliv_numb       = me->av_vbeln.
        w_item_control-deliv_item       = w_item_data-deliv_item.
*       w_item_control-chg_delqty       = 'X'.
*       w_item_control-volume_flg       = 'X'.
*       w_item_control-net_wt_flg       = 'X'.
*       w_item_control-gross_wt_flg     = 'X'.
        APPEND w_item_control TO t_item_control.
        "
        w_item_data_spl-deliv_numb      = me->av_vbeln.
        w_item_data_spl-deliv_item      = w_item_data-deliv_item.
        w_item_data_spl-stge_loc        = wa_dados_processa_po-lgort.
        APPEND w_item_data_spl TO t_item_data_spl.
      ENDLOOP.

      CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
        EXPORTING
          header_data    = w_header_data
          header_control = w_header_control
          delivery       = me->av_vbeln
        TABLES
          header_partner = t_header_partner
          item_data      = t_item_data
          item_control   = t_item_control
          return         = t_retorno2
          item_data_spl  = t_item_data_spl.

      nlinhas = 0.
      DESCRIBE TABLE t_retorno2 LINES nlinhas.

      IF nlinhas IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ELSE.
        LOOP AT t_retorno2 INTO DATA(wa_retorno2).
          CLEAR wa_log_remessa.
          wa_log_remessa-ebeln    = wa_dados_po-ebeln.
          wa_log_remessa-tipo_msg = wa_retorno2-type.
          wa_log_remessa-number   = wa_retorno2-number.

          CALL FUNCTION 'MESSAGE_TEXT_BUILD'
            EXPORTING
              msgid               = wa_retorno2-id
              msgnr               = wa_retorno2-number
              msgv1               = wa_retorno2-message_v1
              msgv2               = wa_retorno2-message_v2
              msgv3               = wa_retorno2-message_v3
              msgv4               = wa_retorno2-message_v4
            IMPORTING
              message_text_output = wa_log_remessa-message.

          APPEND wa_log_remessa TO e_log_remessa.
        ENDLOOP.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        LOOP AT items_particionar INTO DATA(wa_itens).
          zcl_frete_remessa_trans=>zif_frete_remessa_trans~set_criar_log( EXPORTING i_vbeln      = me->av_vbeln
                                                                                    i_ebeln      = CONV #( wa_itens-ebeln )
                                                                                    i_ebelp      = CONV #( wa_itens-ebelp )
                                                                                    i_etapa_proc = '02' "C_ETAPA_CRIAR_REMESSA
                                                                                    i_commit     = abap_true
                                                                          CHANGING  t_return     = me->retorno[] ).
        ENDLOOP.

        me->set_elimina_remessa( me->av_vbeln ).
        RETURN.
      ENDIF.
    ENDLOOP.  "    LOOP AT t_items INTO DATA(w_items).

*-------------------------------------------------
*-- picking
*-------------------------------------------------
    LOOP AT t_items INTO w_items.

      SELECT SINGLE vgbel, vgpos
        FROM lips
        INTO @w_lips
       WHERE vbeln = @w_items-deliv_numb.

      me->set_nr_remessa( i_remessa = w_items-deliv_numb ).

      DATA(lc_gerou_piking) = me->set_piking( EXPORTING t_itens       = items_particionar
                                              IMPORTING e_log_remessa = e_log_remessa ).

      IF lc_gerou_piking = abap_true.
        CALL FUNCTION 'DEQUEUE_ALL'
          EXPORTING
            _synchron = 'X'.

        LOOP AT gt_dados_processa_po INTO wa_dados_processa_po WHERE ebeln = w_lips-vgbel
                                                                 AND ebelp = w_lips-vgpos+1(5).
          wa_dados_remessa-rem_vbeln    =  me->av_vbeln.
          wa_dados_remessa-ped_ebeln    =  wa_dados_processa_po-ebeln.
          wa_dados_remessa-rem_brgew    =  wa_dados_processa_po-menge.
          APPEND wa_dados_remessa      TO e_dados_remessa.
        ENDLOOP.
      ELSE.
        me->set_elimina_remessa( me->av_vbeln ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD set_elimina_remessa.

    DATA: sl_hdata    TYPE bapiobdlvhdrchg,
          sl_hcont    TYPE bapiobdlvhdrctrlchg,
          vl_delivery TYPE bapiobdlvhdrchg-deliv_numb,
          tl_bapiret2 TYPE bapiret2_t.

    sl_hdata-deliv_numb = i_vbeln.
    sl_hcont-deliv_numb = i_vbeln.
    sl_hcont-dlv_del    = abap_true.
    vl_delivery         = i_vbeln.

    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
      EXPORTING
        header_data    = sl_hdata
        header_control = sl_hcont
        delivery       = vl_delivery
      TABLES
        return         = tl_bapiret2.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

  ENDMETHOD.


  METHOD set_gerar_remessa.

    FREE: e_dados_remessa.

    me->set_limpar( ).
    me->cabecalho       = i_cabecalho.
    me->items           = i_items.
    me->parceiros       = i_parceiros.
    me->gerar_movimento = i_gerar_movimento.
    me->particao_lote   = i_particao_lote.

    CASE i_tipo_delivery.
      WHEN 'E'.
        r_gerou = me->set_criar_entrada( IMPORTING e_dados_remessa = e_dados_remessa ).
      WHEN 'S'.
    ENDCASE.

  ENDMETHOD.


  METHOD set_limpar.

    CLEAR: me->av_vbeln,
           me->cabecalho,
           me->items,
           me->parceiros.

  ENDMETHOD.


  METHOD set_lote_item.

    READ TABLE me->lotes ASSIGNING FIELD-SYMBOL(<fs_lote>) WITH KEY cd_lote_item = i_lote-cd_lote_item.

    CHECK <fs_lote> IS ASSIGNED.

    MOVE-CORRESPONDING i_lote TO <fs_lote>.

    "Verificar se Lote Existe
    IF i_lote-charg IS NOT INITIAL.

      TRY.
          CALL METHOD zcl_charg=>get_charg
            EXPORTING
              i_matnr = i_lote-matnr
              i_charg = i_lote-charg
            RECEIVING
              r_mch1  = DATA(r_mch1).

          CALL METHOD zcl_charg=>get_charg_detalhe
            EXPORTING
              i_matnr         = i_lote-matnr
              i_charg         = i_lote-charg
              i_werks         = i_lote-werks
            IMPORTING
              e_ymcha         = DATA(e_ymcha)
              e_classname     = DATA(e_classname)
            RECEIVING
              r_char_of_batch = DATA(r_char_of_batch).

          <fs_lote>-cuobj = e_ymcha-cuobj_bm.
          <fs_lote>-herkl = e_ymcha-herkl.
          <fs_lote>-hsdat = e_ymcha-hsdat.
          <fs_lote>-licha = e_ymcha-licha.
          <fs_lote>-vfdat = e_ymcha-vfdat.
          <fs_lote>-class = e_classname.

          SELECT SINGLE * INTO @DATA(wa_klah)
            FROM klah
           WHERE class EQ @e_classname.

          IF sy-subrc IS INITIAL.
            <fs_lote>-clint = wa_klah-clint.
            <fs_lote>-klart = wa_klah-klart.
          ENDIF.

          MOVE-CORRESPONDING <fs_lote> TO i_lote.

          LOOP AT i_lote_caract ASSIGNING FIELD-SYMBOL(<fs_lotec>) WHERE cd_lote_item EQ i_lote-cd_lote_item.

*            READ TABLE R_CHAR_OF_BATCH WITH KEY ATNAM = <FS_LOTEC>-ATNAM INTO DATA(BP_CARAC).
*            IF SY-SUBRC IS INITIAL.
*              <FS_LOTEC>-ATWRT = BP_CARAC-ATWTB.
*            ENDIF.

            READ TABLE me->lotes_caracteristicas ASSIGNING FIELD-SYMBOL(<fs_carac>)
              WITH KEY cd_lote_item = <fs_lotec>-cd_lote_item
                       atinn        = <fs_lotec>-atinn.

            IF sy-subrc IS INITIAL.
              MOVE-CORRESPONDING <fs_lotec> TO <fs_carac>.
            ENDIF.

          ENDLOOP.

          me->ck_alterou = abap_true.
          me->ck_alterou_lotes = abap_true.
          EXIT.

        CATCH zcx_charg_exception .
      ENDTRY.
    ENDIF.

    LOOP AT i_lote_caract INTO DATA(wa_carac) WHERE cd_lote_item EQ i_lote-cd_lote_item.
      READ TABLE me->lotes_caracteristicas ASSIGNING FIELD-SYMBOL(<fs_carac2>)
       WITH KEY cd_lote_item = wa_carac-cd_lote_item
                atinn        = wa_carac-atinn.
      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING wa_carac TO <fs_carac2>.
      ENDIF.
    ENDLOOP.

    me->ck_alterou = abap_true.
    me->ck_alterou_lotes = abap_true.

  ENDMETHOD.


  METHOD set_nr_remessa.

    r_delivery = me.
    me->av_vbeln = i_remessa.

  ENDMETHOD.


  METHOD set_piking.

    DATA: lc_vbkok_wa    TYPE vbkok,
          it_vbpok_tab   TYPE TABLE OF vbpok,
          wa_vbpok_tab   TYPE vbpok,
          wa_log_remessa TYPE zlest0247,
          it_prot        TYPE TABLE OF prott,
          lc_retorno     TYPE TABLE OF bapiret2,
          wa_retorno     TYPE bapiret2.

    SELECT SINGLE * INTO @DATA(wa_likp)
      FROM likp
     WHERE vbeln EQ @me->av_vbeln.

    IF wa_likp-wadat_ist IS NOT INITIAL.
      r_gerou = abap_true.
      EXIT.
    ENDIF.

    lc_vbkok_wa-vbeln_vl = me->av_vbeln.
    lc_vbkok_wa-vbeln    = me->av_vbeln.
    lc_vbkok_wa-wabuc    = abap_true.
    lc_vbkok_wa-kodat    = sy-datlo.

    SELECT * INTO TABLE @DATA(it_lips)
      FROM lips
     WHERE vbeln EQ @me->av_vbeln.

    LOOP AT it_lips INTO DATA(wa_lips).
      wa_vbpok_tab-vbeln_vl = wa_lips-vbeln.
      wa_vbpok_tab-posnr_vl = wa_lips-posnr.
      wa_vbpok_tab-vbeln    = wa_lips-vbeln.
      wa_vbpok_tab-posnn    = wa_lips-posnr.
      wa_vbpok_tab-vfdat    = sy-datum. "me->at_vfdat.
      wa_vbpok_tab-pikmg    = wa_lips-lfimg.
      wa_vbpok_tab-brgew    = wa_lips-lfimg.
      wa_vbpok_tab-ntgew    = wa_lips-lfimg.
      wa_vbpok_tab-gewei    = 'KG'.
      wa_vbpok_tab-vbtyp_n  = 'V'.
      APPEND wa_vbpok_tab  TO it_vbpok_tab.
    ENDLOOP.

    CALL FUNCTION 'SD_DELIVERY_UPDATE_PICKING_1'
      EXPORTING
        vbkok_wa  = lc_vbkok_wa
        synchron  = abap_true
      TABLES
        vbpok_tab = it_vbpok_tab
        prot      = it_prot.

    LOOP AT it_prot INTO DATA(wa_prot).
      wa_retorno-id         = wa_prot-msgid.
      wa_retorno-number     = wa_prot-msgno.
      wa_retorno-type       = wa_prot-msgty.
      wa_retorno-message_v1 = wa_prot-msgv1.
      wa_retorno-message_v2 = wa_prot-msgv2.
      wa_retorno-message_v3 = wa_prot-msgv3.
      wa_retorno-message_v4 = wa_prot-msgv4.
      MESSAGE ID     wa_prot-msgid
              TYPE   wa_prot-msgty
              NUMBER wa_prot-msgno
              WITH   wa_prot-msgv1 wa_prot-msgv2 wa_prot-msgv3 wa_prot-msgv4
              INTO   wa_retorno-message.
      APPEND wa_retorno TO lc_retorno.
    ENDLOOP.

    READ TABLE lc_retorno WITH KEY type = 'E' INTO wa_retorno.

    IF sy-subrc IS INITIAL.
      r_gerou = abap_false.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      me->set_retorno( i_retorno = lc_retorno[] ).

      LOOP AT t_itens INTO DATA(wa_itens).
        LOOP AT lc_retorno INTO wa_retorno.
          CLEAR wa_log_remessa.
          wa_log_remessa-ebeln    = wa_itens-ebeln.
          wa_log_remessa-tipo_msg = wa_retorno-type.
          wa_log_remessa-number   = wa_retorno-number.
          wa_log_remessa-message  = wa_retorno-message.
          APPEND wa_log_remessa  TO e_log_remessa.
        ENDLOOP.

        zcl_frete_remessa_trans=>zif_frete_remessa_trans~set_criar_log( EXPORTING i_vbeln      = me->av_vbeln
                                                                                  i_ebeln      = CONV #( wa_itens-ebeln )
                                                                                  i_ebelp      = CONV #( wa_itens-ebelp )
                                                                                  i_etapa_proc = '02' "C_ETAPA_CRIAR_REMESSA
                                                                                  i_commit     = abap_true
                                                                        CHANGING  t_return     = me->retorno[] ).
      ENDLOOP.
    ELSE.
      r_gerou = abap_true.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    ENDIF.

  ENDMETHOD.


  METHOD set_retorno.

    me->retorno = i_retorno.

  ENDMETHOD.
ENDCLASS.
