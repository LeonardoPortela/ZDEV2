class ZCL_REMESSA_TERCEIRO definition
  public
  final
  create public .

public section.

  interfaces ZIF_REMESSA_TERCEIRO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_REMESSA_TERCEIRO IMPLEMENTATION.


  METHOD zif_remessa_terceiro~get_dados_transporte.

    FREE: e_placa,     e_quantidade, e_tp_frete, e_itinerario,
          e_vlr_frete, e_unid_cond,  e_dados_transp,
          e_lock_ag_frete.

*-----------------------------------
* recupera informacoes
*-----------------------------------
    SELECT *
      INTO @DATA(w_zlest0211)
      FROM zlest0211
        UP TO 1 ROWS
     WHERE vbeln = @i_remessa_dummy.
    ENDSELECT.

    SELECT ktokk
      INTO @DATA(l_ktokk)
      FROM lfa1
        UP TO 1 ROWS
     WHERE lifnr = @w_zlest0211-agente_frete.
    ENDSELECT.

    SELECT route, kwmeng
      INTO TABLE @DATA(t_vbap)
      FROM vbap
     WHERE vbeln = @i_ov_dummy.

    SELECT lfimg, gewei
      INTO TABLE @DATA(t_lips)
      FROM lips
     WHERE vbeln = @i_remessa_dummy.

    SELECT inco1
      INTO @DATA(l_tp_frete)
      FROM vbkd
        UP TO 1 ROWS
     WHERE vbeln = @i_ov_dummy.
    ENDSELECT.

*-----------------------------------
* atribuir informacoes
*-----------------------------------
    READ TABLE t_vbap INTO DATA(w_vbap) INDEX 1.

    DATA(l_quant) = 0.
    LOOP AT t_lips INTO DATA(w_lips).
      l_quant     = l_quant + w_lips-lfimg.
    ENDLOOP.

*   IF l_ktokk IS NOT INITIAL.
*     IF l_ktokk = 'ZFIC'.
*       e_tp_frete   = 'CIF'.
*     ELSE.
*       e_tp_frete   = 'CPT'.
*     ENDIF.
*   ENDIF.

    e_tp_frete       = l_tp_frete.
    e_placa          = w_zlest0211-placa_cav.
    e_quantidade     = l_quant.
    e_itinerario     = w_vbap-route.
    e_vlr_frete      = w_zlest0211-vlr_frete.
    e_unid_cond      = w_zlest0211-unid_cond.

*   IF l_tp_frete <> 'CIF'.              "*-CS2024000522-29.08.2024-JT-#150113-inicio
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


  METHOD zif_remessa_terceiro~get_instance.

    IF zif_remessa_terceiro~at_remessa_terceiro IS NOT BOUND.
      CREATE OBJECT zif_remessa_terceiro~at_remessa_terceiro TYPE zcl_remessa_terceiro.
      r_instancia = zif_remessa_terceiro~at_remessa_terceiro.
    ELSE.
      r_instancia = zif_remessa_terceiro~at_remessa_terceiro.
    ENDIF.

  ENDMETHOD.


  METHOD zif_remessa_terceiro~get_status_nf_remessa.

    DATA: w_campos_nfe TYPE zde_campos_nfe,
          zcl_util     TYPE REF TO zcl_util.

    FREE: e_nf_remessa.

    CREATE OBJECT zcl_util.

    SELECT chave_nf_cta_ordem
      FROM zlest0210
        UP TO 1 ROWS
      INTO @DATA(l_chave_nf_cta_ordem)
     WHERE chave_nf_venda = @i_chave_nfe.
    ENDSELECT.

    w_campos_nfe = zcl_util->get_atributos_nfe( l_chave_nf_cta_ordem ).

    IF w_campos_nfe-nfnum9 IS INITIAL.
      e_nf_remessa = icon_linked_document.
    ELSE.
      e_nf_remessa = w_campos_nfe-nfnum9.
    ENDIF.

  ENDMETHOD.


  METHOD zif_remessa_terceiro~get_status_outros_docs.

    DATA: l_icon         TYPE icon-id,
          lv_doc_custo   TYPE fknum,
          lv_ordem_serv	 TYPE vbeln_va,
          lv_fatura_serv TYPE vbeln_vf,
          lv_dacte       TYPE j_1bdocnum,
          lv_st_proc     TYPE zst_proc.

    FREE:  e_doc_custo,  e_ordem_serv,  e_fatura_serv,  e_dacte,
          lv_doc_custo, lv_ordem_serv, lv_fatura_serv, lv_dacte,
          lv_st_proc.

*--------------------------
*-- documentos gerados
*--------------------------
    SELECT fknum, ov_frete, fatura_frete, nro_nf_frete, doc_transp
      FROM zlest0211
      INTO @DATA(w_zlest0211)
        UP TO 1 ROWS
     WHERE vbeln = @i_remessa_dummy.
    ENDSELECT.

    IF sy-subrc <> 0.
      e_doc_custo     = icon_icon_list.
      e_ordem_serv    = icon_icon_list.
      e_fatura_serv   = icon_icon_list.
      e_dacte         = icon_icon_list.
      EXIT.
    ENDIF.

*--------------------------
*-- icoterms
*--------------------------
    SELECT inco1
      INTO @DATA(l_tp_frete)
      FROM vbkd
        UP TO 1 ROWS
     WHERE vbeln = @i_ov_dummy.
    ENDSELECT.

    IF l_tp_frete = 'CPT'.
      l_icon = icon_text_ina.
    ELSE.
      l_icon = icon_icon_list.
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

*--------------------------
*-- Ajusta Status documentos
*--------------------------
    IF l_tp_frete = 'CIF'.
      IF e_doc_custo IS NOT INITIAL AND e_doc_custo(1) <> '@'.
        lv_doc_custo   = e_doc_custo.
        lv_st_proc     = '05'.
      ENDIF.
      IF e_ordem_serv IS NOT INITIAL AND e_ordem_serv(1) <> '@'.
        lv_ordem_serv  = e_ordem_serv.
        lv_st_proc     = '06'.
      ENDIF.
      IF e_fatura_serv IS NOT INITIAL AND e_fatura_serv(1) <> '@'.
        lv_fatura_serv = e_fatura_serv.
        lv_st_proc     = '07'.
      ENDIF.
      IF e_dacte IS NOT INITIAL AND e_dacte(1) <> '@'.
        lv_dacte       = e_dacte.
        lv_st_proc     = '99'.
      ENDIF.
    ELSE.
      IF e_doc_custo IS NOT INITIAL AND e_doc_custo(1) <> '@'.
        lv_doc_custo   = e_doc_custo.
        lv_st_proc     = '99'.
      ENDIF.
    ENDIF.

    CHECK lv_doc_custo   IS NOT INITIAL OR
          lv_ordem_serv  IS NOT INITIAL OR
          lv_fatura_serv IS NOT INITIAL OR
          lv_dacte       IS NOT INITIAL OR
          lv_st_proc     IS NOT INITIAL.

*--------------------------
*-- Ajusta tabela
*--------------------------
    UPDATE zlest0211 SET fknum         = lv_doc_custo
                         ov_frete      = lv_ordem_serv
                         fatura_frete  = lv_fatura_serv
                         nro_nf_frete  = lv_dacte
                         st_proc       = lv_st_proc
                   WHERE vbeln         = i_remessa_dummy.

    COMMIT WORK.

  ENDMETHOD.


  METHOD zif_remessa_terceiro~get_status_ov_dummy.

    FREE: e_ov_dummy.

*--------------------------
*-- checa se tem erro processamento
*--------------------------
    SELECT vbeln
      INTO @DATA(l_vbeln)
      FROM zlest0214
        UP TO 1 ROWS
     WHERE vbeln      = @i_vbeln
       AND etapa_proc = @zif_remessa_terceiro~c_etapa_criar_ov_dummy
       AND msgtyp     = @zif_remessa_terceiro~c_e.
    ENDSELECT.

    IF sy-subrc = 0.
      e_ov_dummy = icon_message_error.
    ELSE.
      SELECT vbeln
        INTO l_vbeln
        FROM vbfa
          UP TO 1 ROWS
       WHERE vbelv   = i_vbeln
         AND vbtyp_n = zif_remessa_terceiro~c_c.
      ENDSELECT.

      IF sy-subrc = 0.
        SELECT auart
          INTO @DATA(l_auart)
          FROM vbak
         UP TO 1 ROWS
         WHERE vbeln = @l_vbeln.
        ENDSELECT.

        IF sy-subrc = 0 AND l_auart = zif_remessa_terceiro~c_zfnt.
          e_ov_dummy = l_vbeln.
        ELSE.
          e_ov_dummy = icon_execute_object.
          UPDATE zlest0210 SET ov_dummy       = abap_off
                         WHERE chave_nf_venda = i_chave_nf_venda.

        ENDIF.
      ELSE.
        e_ov_dummy = icon_execute_object.
        UPDATE zlest0210 SET ov_dummy       = abap_off
                       WHERE chave_nf_venda = i_chave_nf_venda.
      ENDIF.
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.


  METHOD zif_remessa_terceiro~get_status_remessa_dummy.

    FREE: e_remessa_dummy.

*--------------------------
*-- checa se tem erro processamento
*--------------------------
    SELECT vbeln
      INTO @DATA(l_vbeln)
      FROM zlest0214
        UP TO 1 ROWS
     WHERE vbeln      = @i_vbeln_venda
       AND nf_venda   = @i_nf_venda
       AND etapa_proc = @zif_remessa_terceiro~c_etapa_criar_remessa_dummy
       AND msgtyp     = @zif_remessa_terceiro~c_e.
    ENDSELECT.

    IF sy-subrc = 0.
      e_remessa_dummy = icon_message_error.
    ELSE.
*     SELECT vbeln
*       INTO l_vbeln
*       FROM vbfa
*         UP TO 1 ROWS
*      WHERE vbelv   = i_vbeln_dummy
*        AND vbtyp_n = zif_remessa_terceiro~c_j.
*     ENDSELECT.
*
      SELECT remessa_dummy
        INTO @DATA(l_remessa_dummy)
        FROM zlest0210
          UP TO 1 ROWS
       WHERE chave_nf_venda  = @i_chave_nf_venda.
      ENDSELECT.

      IF sy-subrc = 0.
        SELECT lfart
          INTO @DATA(l_lfart)
          FROM likp
         UP TO 1 ROWS
         WHERE vbeln = @l_remessa_dummy.
        ENDSELECT.

        IF sy-subrc = 0 AND l_lfart = zif_remessa_terceiro~c_zlf.
          e_remessa_dummy = l_remessa_dummy.
          UPDATE zlest0211 SET st_proc    = '01'
                         WHERE vbeln      = l_remessa_dummy.

        ELSE.
          e_remessa_dummy = icon_execute_object.
          UPDATE zlest0211 SET st_proc        = '00'
                         WHERE vbeln          = l_remessa_dummy.
          UPDATE zlest0210 SET remessa_dummy  = abap_off
                         WHERE chave_nf_venda = i_chave_nf_venda.
        ENDIF.
      ELSE.
        e_remessa_dummy = icon_execute_object.
      ENDIF.
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.


  METHOD zif_remessa_terceiro~get_status_vt.

    FREE: e_transp.

*--------------------------
*-- checa se tem erro processamento
*--------------------------
    SELECT vbeln
      INTO @DATA(l_vbeln)
      FROM zlest0214
        UP TO 1 ROWS
     WHERE vbeln      = @i_vbeln_venda
       AND nf_venda   = @i_nf_venda
       AND etapa_proc = @zif_remessa_terceiro~c_etapa_gerar_vt
       AND msgtyp     = @zif_remessa_terceiro~c_e.
    ENDSELECT.

    IF sy-subrc = 0.
      e_transp = icon_message_error.
      UPDATE zlest0211 SET doc_transp = abap_off
                           st_proc    = '03'
                     WHERE vbeln = i_remessa_dummy.
    ELSE.
      SELECT vbeln
        INTO l_vbeln
        FROM vbfa
          UP TO 1 ROWS
       WHERE vbelv   = i_remessa_dummy
         AND vbtyp_n = zif_remessa_terceiro~c_8
         AND vbtyp_v = zif_remessa_terceiro~c_j.
      ENDSELECT.
*
      IF sy-subrc = 0.
        e_transp = l_vbeln.
        UPDATE zlest0211 SET doc_transp = l_vbeln
                             st_proc    = '04'
                       WHERE vbeln      = i_remessa_dummy.
      ELSE.
        e_transp = icon_execute_object.
        UPDATE zlest0211 SET doc_transp = abap_off
                             st_proc    = '03'
                       WHERE vbeln      = i_remessa_dummy.
      ENDIF.
    ENDIF.

*-CS2024000522-10.09.2024-JT-#151259-inicio
    SELECT SINGLE shtyp
      INTO @DATA(_shtyp)
      FROM vttk
     WHERE tknum = @e_transp.

    IF sy-subrc = 0.
      e_shtyp    = _shtyp.
    ENDIF.
*-CS2024000522-10.09.2024-JT-#151259-fim

    COMMIT WORK.

  ENDMETHOD.


  METHOD zif_remessa_terceiro~get_tab_return.

    t_return[] = zif_remessa_terceiro~at_t_return[].

  ENDMETHOD.


  METHOD zif_remessa_terceiro~set_bdc_data.

    CLEAR zif_remessa_terceiro~w_bdcdata.

    zif_remessa_terceiro~w_bdcdata-program   = i_prog.
    zif_remessa_terceiro~w_bdcdata-dynpro    = i_dynpro.
    zif_remessa_terceiro~w_bdcdata-dynbegin  = i_start.
    zif_remessa_terceiro~w_bdcdata-fnam      = i_fnam.
    zif_remessa_terceiro~w_bdcdata-fval      = i_fval.
    APPEND zif_remessa_terceiro~w_bdcdata   TO zif_remessa_terceiro~t_bdcdata.

  ENDMETHOD.


  METHOD zif_remessa_terceiro~set_calcula_frete.

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

    CLEAR: w_a900, w_a910, w_a911, w_a915, w_a918, w_a919,
           l_cont_fre,
           l_add01,
           e_valor_frete,
           e_unid_cond.

*--------------------------------------
*-- log de proc
*--------------------------------------
    zif_remessa_terceiro~set_free_return( ).
    zif_remessa_terceiro~set_free_log_proc( EXPORTING i_vbeln      = i_vbeln_venda
                                                      i_nf_venda   = i_nf_venda
                                                      i_etapa_proc = zif_remessa_terceiro~c_etapa_calculo_frete ).


*--------------------------------------
*-- informacoes OV venda
*--------------------------------------
    SELECT auart
      INTO @DATA(l_auart)
      FROM vbak
        UP TO 1 ROWS
     WHERE vbeln = @i_vbeln_venda.
    ENDSELECT.

*-CS2024000522-18.07.2024-JT-#143588-inicio
    SELECT inco1
      INTO @DATA(l_inco1)
      FROM vbkd
        UP TO 1 ROWS
     WHERE vbeln = @i_vbeln_venda.
    ENDSELECT.
*-CS2024000522-18.07.2024-JT-#143588-fim

    SELECT route
      INTO @DATA(l_route)
      FROM vbap
        UP TO 1 ROWS
     WHERE vbeln = @i_vbeln_venda.
    ENDSELECT.

    SELECT shtyp
      INTO @DATA(l_shtyp)
      FROM zsdt0011
        UP TO 1 ROWS
     WHERE auart = @l_auart.
    ENDSELECT.

*--------------------------------------
*-- remessa
*--------------------------------------
    SELECT matnr
      INTO @DATA(l_matnr)
      FROM lips
        UP TO 1 ROWS
     WHERE vbeln = @i_remessa_dummy.
    ENDSELECT.

*--------------------------------------
*-- informacoes transporte
*--------------------------------------
    SELECT *
      INTO @DATA(w_zlest0211)
      FROM zlest0211
        UP TO 1 ROWS
      WHERE vbeln = @i_remessa_dummy.
    ENDSELECT.

*--------------------------------------
*-- fornecedor
*--------------------------------------
    SELECT *
      INTO @DATA(w_lfa1)
      FROM lfa1
        UP TO 1 ROWS
      WHERE lifnr = @w_zlest0211-cod_loc_coleta.
    ENDSELECT.

*--------------------------------------
*-- cliente
*--------------------------------------
    SELECT *
      INTO @DATA(w_kna1)
      FROM kna1
        UP TO 1 ROWS
      WHERE kunnr = @w_zlest0211-cod_loc_entrega.
    ENDSELECT.

*--------------------------------------
*-- informacoes transporte
*--------------------------------------
    IF l_inco1 <> 'CPT'.  "*-CS2024000522-18.07.2024-JT-#143588
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

          zif_remessa_terceiro~set_tab_return( i_type = 'E' i_id = 'SD' i_number = '024' i_message = l_mensagem ).
          t_return = zif_remessa_terceiro~get_tab_return( ).
          zif_remessa_terceiro~set_criar_log( EXPORTING i_vbeln      = i_vbeln_venda
                                                        i_nf_venda   = i_nf_venda
                                                        i_etapa_proc = zif_remessa_terceiro~c_etapa_calculo_frete
                                                        i_commit     = abap_true
                                               CHANGING t_return     = t_return[] ).

          RAISE EXCEPTION TYPE zcx_remessa_terceiro
            EXPORTING
              textid = VALUE #( msgid  = zcx_remessa_terceiro=>zcx_erro_geral-msgid
                                msgno  = zcx_remessa_terceiro=>zcx_erro_geral-msgno
                                attr1  = CONV #( sy-msgv1 ) attr2  = CONV #( sy-msgv2 ) attr3  = CONV #( sy-msgv3 ) attr4  = CONV #( sy-msgv4 ) )
              msgty  = 'E'
              msgid  = zcx_remessa_terceiro=>zcx_erro_geral-msgid
              msgno  = zcx_remessa_terceiro=>zcx_erro_geral-msgno
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
      zif_remessa_terceiro~set_tab_return( i_type = 'W' i_id = 'SD' i_number = '024' i_message = 'Frete não foi calculado' ).
      t_return = zif_remessa_terceiro~get_tab_return( ).
      zif_remessa_terceiro~set_criar_log( EXPORTING i_vbeln      = i_vbeln_venda
                                                    i_nf_venda   = i_nf_venda
                                                    i_etapa_proc = zif_remessa_terceiro~c_etapa_calculo_frete
                                                    i_commit     = abap_true
                                           CHANGING t_return     = t_return[] ).
    ELSE.
      l_mensagem = 'Frete calculado. Valor R$' && e_valor_frete.
      zif_remessa_terceiro~set_tab_return( i_type = 'S' i_id = 'SD' i_number = '024' i_message = l_mensagem ).
      t_return = zif_remessa_terceiro~get_tab_return( ).
      zif_remessa_terceiro~set_criar_log( EXPORTING i_vbeln      = i_vbeln_venda
                                                    i_nf_venda   = i_nf_venda
                                                    i_etapa_proc = zif_remessa_terceiro~c_etapa_calculo_frete
                                                    i_commit     = abap_true
                                           CHANGING t_return     = t_return[] ).


    ENDIF.

  ENDMETHOD.


  METHOD zif_remessa_terceiro~set_change_agente_frete.

    UPDATE zlest0211 SET agente_frete = i_ag_frete
                   WHERE vbeln        = i_remessa_dummy.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD zif_remessa_terceiro~set_criar_log.

    DATA: w_zlest0214 TYPE zlest0214,
          l_timestamp TYPE timestampl,
          l_message   TYPE bdc_vtext1.

*--------------------------
*-- elimina msg duplicadas
*--------------------------
    SORT t_return BY type id number.
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
           AND posnr = @i_posnr
           AND cont  = @l_timestamp.
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
      w_zlest0214-mandt       = sy-mandt.
      w_zlest0214-vbeln       = i_vbeln.
      w_zlest0214-posnr       = i_posnr.
      w_zlest0214-nf_venda    = i_nf_venda.
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


  METHOD zif_remessa_terceiro~set_cria_doc_custo.

    DATA: l_tknum        TYPE tknum,
          l_mensagem     TYPE char255,
          l_fknum        TYPE zlest0211-fknum,
          l_ov_frete     TYPE zlest0211-ov_frete,
          l_fatura_frete TYPE zlest0211-fatura_frete,
          l_ebeln        TYPE ekko-ebeln,
          l_st_proc      TYPE zst_proc,
          l_dacte        TYPE j_1bdocnum,
          t_return       TYPE TABLE OF bapiret2.

    r_instancia = me.

    FREE: l_fknum,
          l_ov_frete,
          l_st_proc,
          l_fatura_frete,
          e_doc_custo,
          e_ordem_serv,
          e_fatura_serv,
          e_dacte.

*--------------------------------------
*-- log de proc
*--------------------------------------
    zif_remessa_terceiro~set_free_return( ).
    zif_remessa_terceiro~set_free_log_proc( EXPORTING i_vbeln      = i_vbeln_venda
                                                      i_nf_venda   = i_nf_venda
                                                      i_etapa_proc = zif_remessa_terceiro~c_etapa_gerar_doc_custo ).
    zif_remessa_terceiro~set_free_log_proc( EXPORTING i_vbeln      = i_vbeln_venda
                                                      i_nf_venda   = i_nf_venda
                                                      i_etapa_proc = zif_remessa_terceiro~c_etapa_gerar_ordem_serv ).
    zif_remessa_terceiro~set_free_log_proc( EXPORTING i_vbeln      = i_vbeln_venda
                                                      i_nf_venda   = i_nf_venda
                                                      i_etapa_proc = zif_remessa_terceiro~c_etapa_gerar_fatura_serv ).

*--------------------------------------
*-- tratar nro transporte
*--------------------------------------
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = i_tknum
      IMPORTING
        output = l_tknum.

*--------------------------------------
*-- emissao documentos
*--------------------------------------
    TRY .
        zcl_faturamento=>zif_faturamento~get_instance(
          )->get_processo_emissao_docs(
            EXPORTING
              i_tknum        = l_tknum
            IMPORTING
              e_doc_custo    = DATA(l_doc_custo)
              e_conhecimento = DATA(l_conhecimento)
              e_tipo_veiculo = DATA(l_tipo_veiculo)
              e_tp_frete     = DATA(l_tp_frete)
              e_manifesto    = DATA(l_manifesto)
          ).

      CATCH zcx_faturamento INTO DATA(ex_faturamento).
        l_mensagem = ex_faturamento->get_longtext( ).
        zif_remessa_terceiro~set_tab_return( i_type = 'E' i_id = 'SD' i_number = '024' i_message = l_mensagem ).
        t_return = zif_remessa_terceiro~get_tab_return( ).
        zif_remessa_terceiro~set_criar_log( EXPORTING i_vbeln      = i_vbeln_venda
                                                      i_nf_venda   = i_nf_venda
                                                      i_etapa_proc = zif_remessa_terceiro~c_etapa_gerar_doc_custo
                                                      i_commit     = abap_true
                                             CHANGING t_return     = t_return[] ).
        RAISE EXCEPTION TYPE zcx_remessa_terceiro
          EXPORTING
            textid = VALUE #( msgid  = zcx_remessa_terceiro=>zcx_erro_criar_doc_custo-msgid
                              msgno  = zcx_remessa_terceiro=>zcx_erro_criar_doc_custo-msgno
                              attr1  = CONV #( i_tknum ) )
            msgty  = 'E'
            msgid  = zcx_remessa_terceiro=>zcx_erro_criar_doc_custo-msgid
            msgno  = zcx_remessa_terceiro=>zcx_erro_criar_doc_custo-msgno
            msgv1  = CONV #( i_tknum ).

      CATCH zcx_error INTO DATA(ex_error).
        l_mensagem = ex_faturamento->get_longtext( ).
        zif_remessa_terceiro~set_tab_return( i_type = 'E' i_id = 'SD' i_number = '024' i_message = l_mensagem ).
        t_return = zif_remessa_terceiro~get_tab_return( ).
        zif_remessa_terceiro~set_criar_log( EXPORTING i_vbeln      = i_vbeln_venda
                                                      i_nf_venda   = i_nf_venda
                                                      i_etapa_proc = zif_remessa_terceiro~c_etapa_gerar_doc_custo
                                                      i_commit     = abap_true
                                             CHANGING t_return     = t_return[] ).
        RAISE EXCEPTION TYPE zcx_remessa_terceiro
          EXPORTING
            textid = VALUE #( msgid  = zcx_remessa_terceiro=>zcx_erro_criar_doc_custo-msgid
                              msgno  = zcx_remessa_terceiro=>zcx_erro_criar_doc_custo-msgno
                              attr1  = CONV #( i_tknum ) )
            msgty  = 'E'
            msgid  = zcx_remessa_terceiro=>zcx_erro_criar_doc_custo-msgid
            msgno  = zcx_remessa_terceiro=>zcx_erro_criar_doc_custo-msgno
            msgv1  = CONV #( i_tknum ).
    ENDTRY.

    IF l_tipo_veiculo EQ zif_faturamento=>st_tp_prop_veiculo_proprio.
      DATA(ckfprop) = abap_true.
    ELSE.
      ckfprop = abap_false.
    ENDIF.

*--------------------------------------
*-- local agente frete
*--------------------------------------
    TRY .
        zcl_fornecedores=>zif_parceiros~get_instance(
          )->set_parceiro( i_parceiro = i_agente_frete
          )->ck_parceiro_local_negocio(
          ).

        DATA(rb_cus) = abap_false.

      CATCH zcx_parceiros.    " .
        IF ckfprop EQ abap_true.
          rb_cus = abap_true.
        ELSEIF ckfprop EQ abap_false AND l_tp_frete EQ zif_carga=>st_tp_frete_cpt.
          rb_cus = abap_true.
        ENDIF.
    ENDTRY.

*--------------------------------------
*-- gerar custo
*--------------------------------------
    SUBMIT zlesr0013 WITH so_tknum = l_tknum
                     WITH p_vbeln  = i_remessa_dummy
                     WITH p_ebeln  = l_ebeln
                     WITH cksetap  = abap_true
                     WITH rb_in    = l_doc_custo    "Documento de custo de frete
                     WITH rb_out   = l_conhecimento "Ordem / Fatura Serviço
                     WITH ckfprop  = ckfprop
                     WITH rb_cus   = rb_cus
                     WITH rb_dtfat = sy-datum
                 AND RETURN.

*--------------------------------------
*-- obter documentos
*--------------------------------------
    GET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD l_fknum.
    GET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD l_ov_frete.
    GET PARAMETER ID 'Z_MY_PARAMETER_3' FIELD l_fatura_frete.

*--------------------------------------
*-- tratar numero doctos
*--------------------------------------
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = l_fknum
      IMPORTING
        output = l_fknum.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = l_ov_frete
      IMPORTING
        output = l_ov_frete.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = l_fatura_frete
      IMPORTING
        output = l_fatura_frete.

*-----------------------------
*-- doc custo
*-----------------------------
    IF l_fknum IS NOT INITIAL.
      l_st_proc  = '05'.
      l_mensagem = 'Gerado Doc.Custo'.
      zif_remessa_terceiro~set_free_return( ).
      zif_remessa_terceiro~set_tab_return( i_type = 'S' i_id = 'SD' i_number = '024' i_message = l_mensagem ).
    ELSE.
      l_st_proc  = '04'.
      l_mensagem = 'Não foi gerado Documento Custo.'.
      zif_remessa_terceiro~set_free_return( ).
      zif_remessa_terceiro~set_tab_return( i_type = 'E' i_id = 'SD' i_number = '024' i_message = l_mensagem ).
    ENDIF.

    t_return = zif_remessa_terceiro~get_tab_return( ).
    zif_remessa_terceiro~set_criar_log( EXPORTING i_vbeln      = i_vbeln_venda
                                                  i_nf_venda   = i_nf_venda
                                                  i_etapa_proc = zif_remessa_terceiro~c_etapa_gerar_doc_custo
                                                  i_doc_gerado = l_fknum
                                                  i_commit     = abap_false
                                         CHANGING t_return     = t_return[] ).

*-----------------------------
*-- Ov servico
*-----------------------------
    IF l_ov_frete IS NOT INITIAL.
      l_st_proc  = '06'.
      l_mensagem = 'Gerada Ordem Serviço'.
      zif_remessa_terceiro~set_free_return( ).
      zif_remessa_terceiro~set_tab_return( i_type = 'S' i_id = 'SD' i_number = '024' i_message = l_mensagem ).
    ELSE.
      l_mensagem = 'Não foi gerada Ordem de Serviço.'.
      zif_remessa_terceiro~set_free_return( ).
      zif_remessa_terceiro~set_tab_return( i_type = 'E' i_id = 'SD' i_number = '024' i_message = l_mensagem ).
    ENDIF.

    t_return = zif_remessa_terceiro~get_tab_return( ).
    zif_remessa_terceiro~set_criar_log( EXPORTING i_vbeln      = i_vbeln_venda
                                                  i_nf_venda   = i_nf_venda
                                                  i_etapa_proc = zif_remessa_terceiro~c_etapa_gerar_ordem_serv
                                                  i_doc_gerado = l_ov_frete
                                                  i_commit     = abap_false
                                         CHANGING t_return     = t_return[] ).

*-----------------------------
*-- Fatura servico
*-----------------------------
    IF l_fatura_frete IS NOT INITIAL.
      l_st_proc  = '07'.
      l_mensagem = 'Gerada Fatura Serviço'.
      zif_remessa_terceiro~set_free_return( ).
      zif_remessa_terceiro~set_tab_return( i_type = 'S' i_id = 'SD' i_number = '024' i_message = l_mensagem ).
    ELSE.
      l_mensagem = 'Não foi gerada Fatura Serviço'.
      zif_remessa_terceiro~set_free_return( ).
      zif_remessa_terceiro~set_tab_return( i_type = 'E' i_id = 'SD' i_number = '024' i_message = l_mensagem ).
    ENDIF.

    t_return = zif_remessa_terceiro~get_tab_return( ).
    zif_remessa_terceiro~set_criar_log( EXPORTING i_vbeln      = i_vbeln_venda
                                                  i_nf_venda   = i_nf_venda
                                                  i_etapa_proc = zif_remessa_terceiro~c_etapa_gerar_fatura_serv
                                                  i_doc_gerado = l_fatura_frete
                                                  i_commit     = abap_false
                                         CHANGING t_return     = t_return[] ).

*-----------------------------
*-- DACTE
*-----------------------------
    IF l_fatura_frete IS NOT INITIAL.
      DO 3 TIMES.
        SELECT j_1bnfdoc~bukrs, j_1bnflin~docnum
          FROM j_1bnflin
         INNER JOIN j_1bnfdoc ON j_1bnfdoc~docnum = j_1bnflin~docnum
          INTO @DATA(w_doc)
            UP TO 1 ROWS
         WHERE j_1bnflin~refkey EQ @l_fatura_frete.
        ENDSELECT.

        IF sy-subrc <> 0.
          WAIT UP TO 2 SECONDS.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.

    IF sy-subrc = 0 AND l_fatura_frete IS NOT INITIAL.
      e_dacte = w_doc-docnum.
    ENDIF.

    IF e_dacte IS NOT INITIAL.
      l_st_proc  = '99'.
      l_mensagem = 'Gerada DACTE'.
      zif_remessa_terceiro~set_free_return( ).
      zif_remessa_terceiro~set_tab_return( i_type = 'S' i_id = 'SD' i_number = '024' i_message = l_mensagem ).
    ELSE.
      l_mensagem = 'Não foi gerada DACTE'.
      zif_remessa_terceiro~set_free_return( ).
      zif_remessa_terceiro~set_tab_return( i_type = 'E' i_id = 'SD' i_number = '024' i_message = l_mensagem ).
    ENDIF.

    t_return = zif_remessa_terceiro~get_tab_return( ).
    zif_remessa_terceiro~set_criar_log( EXPORTING i_vbeln      = i_vbeln_venda
                                                  i_nf_venda   = i_nf_venda
                                                  i_etapa_proc = zif_remessa_terceiro~c_etapa_gerar_dacte
                                                  i_doc_gerado = e_dacte
                                                  i_commit     = abap_false
                                         CHANGING t_return     = t_return[] ).

*-----------------------------
*-- erro nao gerou doc custo
*-----------------------------
    IF l_fknum IS INITIAL.
      RAISE EXCEPTION TYPE zcx_remessa_terceiro
        EXPORTING
          textid = VALUE #( msgid  = zcx_remessa_terceiro=>zcx_erro_criar_doc_custo-msgid
                            msgno  = zcx_remessa_terceiro=>zcx_erro_criar_doc_custo-msgno
                            attr1  = CONV #( i_tknum ) )
          msgty  = 'E'
          msgid  = zcx_remessa_terceiro=>zcx_erro_criar_doc_custo-msgid
          msgno  = zcx_remessa_terceiro=>zcx_erro_criar_doc_custo-msgno
          msgv1  = CONV #( i_tknum ).
    ENDIF.

*-----------------------------
*-- documentos gerados
*-----------------------------
    e_doc_custo   = l_fknum.
    e_ordem_serv  = l_ov_frete.
    e_fatura_serv = l_fatura_frete.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = e_dacte
      IMPORTING
        output = l_dacte.

*-----------------------------
*-- Atualiza tabela
*-----------------------------
    UPDATE zlest0211 SET fknum        = e_doc_custo
                         ov_frete     = e_ordem_serv
                         fatura_frete = e_fatura_serv
                         nro_nf_frete = l_dacte
                         st_proc      = l_st_proc
                   WHERE vbeln        = i_remessa_dummy.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_on.

  ENDMETHOD.


  METHOD zif_remessa_terceiro~set_cria_ov_dummy.

    DATA: l_vbeln         TYPE vbak-vbeln,
          w_header_in     TYPE bapisdhd1,
          t_items_in      TYPE STANDARD TABLE OF bapisditm,
          w_items_in      TYPE bapisditm,
          t_schedules_in  TYPE STANDARD TABLE OF bapischdl,
          w_schedules_in  TYPE bapischdl,
          t_conditions_in TYPE STANDARD TABLE OF bapicond,
          w_conditions_in TYPE bapicond,
          t_partners      TYPE STANDARD TABLE OF bapiparnr,
          w_partners      TYPE bapiparnr,
          t_bapiparex     TYPE TABLE OF bapiparex,
          w_bapiparex     TYPE bapiparex,
          w_bape_vbak     TYPE bape_vbak,
          w_bape_vbakx    TYPE bape_vbakx,
          t_return        TYPE STANDARD TABLE OF bapiret2.

    r_instancia = me.

    FREE: w_header_in,     w_bape_vbak, w_bape_vbakx,
          t_bapiparex,     t_items_in,  t_schedules_in,
          t_conditions_in, t_partners,  t_return,
          e_ov_dummy,      l_vbeln.

    zif_remessa_terceiro~set_free_return( ).
    zif_remessa_terceiro~set_free_log_proc( EXPORTING i_vbeln      = me->zif_remessa_terceiro~at_vbak-vbeln
                                                      i_nf_venda   = i_nf_venda
                                                      i_etapa_proc = zif_remessa_terceiro~c_etapa_criar_ov_dummy ).

*--------------------------------
*-- header
*--------------------------------
    READ TABLE me->zif_remessa_terceiro~at_vbkd INTO DATA(w_vbkd) INDEX 1.

    w_header_in-ref_doc    = me->zif_remessa_terceiro~at_vbak-vbeln.
    w_header_in-refdoc_cat = zif_remessa_terceiro~c_c.
    w_header_in-doc_type   = zif_remessa_terceiro~c_zfnt.
    w_header_in-purch_no_c = me->zif_remessa_terceiro~at_vbak-bstnk.
    w_header_in-sales_org  = me->zif_remessa_terceiro~at_vbak-vkorg.
    w_header_in-distr_chan = me->zif_remessa_terceiro~at_vbak-vtweg.
    w_header_in-division   = me->zif_remessa_terceiro~at_vbak-spart.
    w_header_in-incoterms1 = w_vbkd-inco1.
    w_header_in-incoterms2 = w_vbkd-inco2.
    w_header_in-pmnttrms   = w_vbkd-zterm.
    w_header_in-pymt_meth  = w_vbkd-zlsch.
    w_header_in-fix_val_dy = w_vbkd-valdt.
    w_header_in-comp_cde_b = me->zif_remessa_terceiro~at_vbak-bukrs_vf.
    w_header_in-ref_doc_l  = me->zif_remessa_terceiro~at_vbak-xblnr.
    w_header_in-dlvschduse = me->zif_remessa_terceiro~at_vbak-abrvw.

*--------------------------------
*-- BAPE_VBAK
*--------------------------------
    w_bape_vbak            = VALUE #(   zpesagem  = zif_remessa_terceiro~c_02 ).
    w_bape_vbakx           = VALUE #(   zpesagem  = abap_true ).
    t_bapiparex[]          = VALUE #( ( structure = 'BAPE_VBAK'  valuepart1 = w_bape_vbak )
                                      ( structure = 'BAPE_VBAKX' valuepart1 = w_bape_vbakx ) ).

*--------------------------------
*-- ITENS
*--------------------------------
    LOOP AT me->zif_remessa_terceiro~at_vbap INTO DATA(w_vbap).

      READ TABLE me->zif_remessa_terceiro~at_konv INTO DATA(w_konv)
                                                  WITH KEY kposn = w_vbap-posnr
                                                           kschl = zif_remessa_terceiro~c_pr00.

      w_items_in-itm_number      = w_vbap-posnr.
      w_items_in-material        = w_vbap-matnr.
      w_items_in-plant           = w_vbap-werks.
      w_items_in-store_loc       = w_vbap-lgort.
      w_items_in-batch           = w_vbap-charg.
      w_items_in-material        = w_vbap-matnr.
      w_items_in-route           = w_vbap-route.
      w_items_in-target_qty      = w_vbap-kwmeng.
      w_items_in-target_qu       = w_vbap-vrkme.
      w_items_in-sales_unit      = w_vbap-vrkme.
      w_items_in-gross_wght      = w_vbap-brgew.
      w_items_in-net_weight      = w_vbap-ntgew.
      w_items_in-untof_wght      = w_vbap-gewei.
*
      w_items_in-ref_doc         = me->zif_remessa_terceiro~at_vbak-vbeln.
      w_items_in-ref_doc_it      = w_vbap-posnr.
      w_items_in-ref_doc_ca      = zif_remessa_terceiro~c_c.
*
      w_items_in-sd_taxcode      = w_vbap-j_1btxsdc.
      w_items_in-taxlawicms      = w_vbap-j_1btaxlw1.
      w_items_in-taxlawipi       = w_vbap-j_1btaxlw2.
      w_items_in-taxlawiss       = w_vbap-j_1btaxlw3.
      w_items_in-taxlawcofins    = w_vbap-j_1btaxlw4.
      w_items_in-taxlawpis       = w_vbap-j_1btaxlw5.
*
      w_items_in-fix_val_dy      = sy-datum.
      w_items_in-price_date      = sy-datum.
      w_items_in-dlvschduse      = w_vbap-vkaus.
      w_items_in-cfop_long       = w_vbap-j_1bcfop.
      APPEND w_items_in         TO t_items_in.

      w_schedules_in-itm_number  = w_vbap-posnr.
      w_schedules_in-req_qty     = w_vbap-kwmeng. "  '1'.
      APPEND w_schedules_in     TO t_schedules_in.

      w_conditions_in-itm_number = w_vbap-posnr.
      w_conditions_in-cond_type  = zif_remessa_terceiro~c_pr00.
      w_conditions_in-cond_value = w_konv-kbetr. "w_vbap-netwr.
      w_conditions_in-cond_unit  = w_konv-kmein. "w_vbap-netwr.
      w_conditions_in-currency   = w_vbap-waerk.
      APPEND w_conditions_in    TO t_conditions_in.

*     w_conditions_in-itm_number = w_vbap-posnr.
*     w_conditions_in-cond_type  = 'ZCOM'.
*     w_conditions_in-cond_value = w_vbap-netwr.
*     APPEND w_conditions_in    TO t_conditions_in.
    ENDLOOP.

*--------------------------------
*-- PARCEIROS
*--------------------------------
    LOOP AT me->zif_remessa_terceiro~at_vbpa INTO DATA(w_vbpa).
      CASE w_vbpa-parvw.
        WHEN 'PC'.
          w_partners-partn_numb = w_vbpa-lifnr.
        WHEN OTHERS.
          w_partners-partn_numb = w_vbpa-kunnr.
      ENDCASE.
      w_partners-partn_role     = w_vbpa-parvw.
      APPEND w_partners        TO t_partners.
    ENDLOOP.

*--------------------------------
*-- EXECUTA BAPI
*--------------------------------
    CALL FUNCTION 'SD_SALESDOCUMENT_CREATE'
      EXPORTING
        sales_header_in     = w_header_in
      IMPORTING
        salesdocument_ex    = l_vbeln
      TABLES
        return              = t_return
        sales_items_in      = t_items_in
        sales_partners      = t_partners
        sales_schedules_in  = t_schedules_in
        sales_conditions_in = t_conditions_in
        extensionin         = t_bapiparex.

*--------------------------------
*-- Criar log
*--------------------------------
    IF l_vbeln IS INITIAL.
      zif_remessa_terceiro~set_criar_log( EXPORTING i_vbeln      = me->zif_remessa_terceiro~at_vbak-vbeln
                                                    i_nf_venda   = i_nf_venda
                                                    i_etapa_proc = zif_remessa_terceiro~c_etapa_criar_ov_dummy
                                                    i_doc_gerado = l_vbeln
                                           CHANGING t_return     = t_return[] ).

      RAISE EXCEPTION TYPE zcx_remessa_terceiro
        EXPORTING
          textid = VALUE #( msgid  = zcx_remessa_terceiro=>zcx_erro_criar_ov_dummy-msgid
                            msgno  = zcx_remessa_terceiro=>zcx_erro_criar_ov_dummy-msgno
                            attr1  = CONV #( me->zif_remessa_terceiro~at_vbak-vbeln ) )
          msgty  = 'E'
          msgid  = zcx_remessa_terceiro=>zcx_erro_criar_ov_dummy-msgid
          msgno  = zcx_remessa_terceiro=>zcx_erro_criar_ov_dummy-msgno
          msgv1  = CONV #( me->zif_remessa_terceiro~at_vbak-vbeln ).
    ELSE.
      zif_remessa_terceiro~set_criar_log( EXPORTING i_vbeln      = me->zif_remessa_terceiro~at_vbak-vbeln
                                                    i_nf_venda   = i_nf_venda
                                                    i_etapa_proc = zif_remessa_terceiro~c_etapa_criar_ov_dummy
                                                    i_doc_gerado = l_vbeln
                                                    i_commit     = abap_false
                                           CHANGING t_return     = t_return[] ).

*-----------------------------
*---- De/Para - NF VeEnda x NF Conta e Ordem
*-----------------------------
      UPDATE zlest0210 SET ov_dummy       = l_vbeln
                     WHERE chave_nf_venda = i_chave_nf_venda.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.

*--------------------------------
*-- documento gerado
*--------------------------------
    e_ov_dummy = l_vbeln.

  ENDMETHOD.


  METHOD zif_remessa_terceiro~set_cria_remessa_dummy.

    DATA: l_ship_point TYPE vstel,
          l_data_rem   TYPE ledat,
          l_delivery   TYPE bapishpdelivnumb-deliv_numb,
          t_item       TYPE TABLE OF bapidlvreftosalesorder,
          w_item       TYPE bapidlvreftosalesorder,
          t_return     TYPE TABLE OF bapiret2,
          w_return     TYPE bapiret2.

    r_instancia = me.

    zif_remessa_terceiro~set_free_return( ).
    zif_remessa_terceiro~set_free_log_proc( EXPORTING i_vbeln      = i_vbeln
                                                      i_nf_venda   = i_nf_venda
                                                      i_etapa_proc = zif_remessa_terceiro~c_etapa_criar_remessa_dummy ).

    FREE: l_delivery,
          t_item,
          t_return,
          e_remessa_dummy.

*--------------------------------
*-- verifica nf
*--------------------------------
    SELECT *
      FROM j_1bnflin
      INTO TABLE @DATA(t_jlin)
     WHERE docnum  = @i_docnum.

    SORT t_jlin BY matnr.

*--------------------------------
*-- ITENS
*--------------------------------
    LOOP AT me->zif_remessa_terceiro~at_vbap INTO DATA(w_vbap).
      READ TABLE t_jlin INTO DATA(w_jlin) WITH KEY matnr = w_vbap-matnr
                                          BINARY SEARCH.
      CHECK sy-subrc = 0.

      l_ship_point      = w_vbap-vstel.
      l_data_rem        = sy-datum.

      w_item-ref_doc    = w_vbap-vbeln.
      w_item-ref_item   = w_vbap-posnr.
      w_item-dlv_qty    = w_jlin-menge.
      w_item-sales_unit = w_vbap-vrkme.
      APPEND w_item    TO t_item.
    ENDLOOP.

*--------------------------------
*-- EXECUTA BAPI
*--------------------------------
"*---> 12/07/2023 - Migração S4 - LO --> Material não foi utilizado
    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_SLS'"#EC CI_USAGE_OK[2438131]
      EXPORTING
        ship_point        = l_ship_point
        due_date          = l_data_rem
      IMPORTING
        delivery          = l_delivery
      TABLES
        sales_order_items = t_item
        return            = t_return.

*--------------------------------
*-- Criar log
*--------------------------------
    IF l_delivery IS INITIAL.
      zif_remessa_terceiro~set_criar_log( EXPORTING i_vbeln      = i_vbeln
                                                    i_nf_venda   = i_nf_venda
                                                    i_etapa_proc = zif_remessa_terceiro~c_etapa_criar_remessa_dummy
                                                    i_doc_gerado = l_delivery
                                           CHANGING t_return     = t_return[] ).

      RAISE EXCEPTION TYPE zcx_remessa_terceiro
        EXPORTING
          textid = VALUE #( msgid  = zcx_remessa_terceiro=>zcx_erro_criar_remessa_dummy-msgid
                            msgno  = zcx_remessa_terceiro=>zcx_erro_criar_remessa_dummy-msgno
                            attr1  = CONV #( i_vbeln ) )
          msgty  = 'E'
          msgid  = zcx_remessa_terceiro=>zcx_erro_criar_remessa_dummy-msgid
          msgno  = zcx_remessa_terceiro=>zcx_erro_criar_remessa_dummy-msgno
          msgv1  = CONV #( i_vbeln ).
    ELSE.
      zif_remessa_terceiro~set_criar_log( EXPORTING i_vbeln      = i_vbeln
                                                    i_nf_venda   = i_nf_venda
                                                    i_etapa_proc = zif_remessa_terceiro~c_etapa_criar_remessa_dummy
                                                    i_doc_gerado = l_delivery
                                                    i_commit     = abap_false
                                           CHANGING t_return     = t_return[] ).

*-----------------------------
*---- De/Para - NF VeEnda x NF Conta e Ordem
*-----------------------------
      UPDATE zlest0210 SET remessa_dummy  = l_delivery
                     WHERE chave_nf_venda = i_chave_nf_venda.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.

*--------------------------------
*-- documento gerado
*--------------------------------
    e_remessa_dummy = l_delivery.

  ENDMETHOD.


  METHOD zif_remessa_terceiro~set_cria_vt.

    DATA: t_return            TYPE TABLE OF bapiret2,
          w_a917              TYPE a917,
          l_mensagem          TYPE char255,
          l_lifnr             TYPE lifnr,
          l_kunnr             TYPE kunnr,
          l_tknum             TYPE vttk-tknum,
*
          st_headerdata       TYPE bapishipmentheader,
          st_stagedata        TYPE bapishipmentstage,
          t_stagedata         TYPE TABLE OF bapishipmentstage,
          t_itemdata          TYPE TABLE OF bapishipmentitem,
          st_itemdata         TYPE bapishipmentitem,
          st_headerdata2      TYPE bapishipmentheader,
          st_headerdataaction TYPE bapishipmentheaderaction,
          t_return_vt         TYPE TABLE OF bapiret2,
          w_return_vt         TYPE bapiret2.

    r_instancia = me.

    FREE: st_headerdata,
          st_stagedata,
          t_stagedata,
          t_itemdata,
          st_itemdata,
          l_tknum,
          t_return_vt,
          e_doc_transp.

*--------------------------------------
*-- log de proc
*--------------------------------------
    zif_remessa_terceiro~set_free_return( ).
    zif_remessa_terceiro~set_free_log_proc( EXPORTING i_vbeln      = i_vbeln_venda
                                                      i_nf_venda   = i_nf_venda
                                                      i_etapa_proc = zif_remessa_terceiro~c_etapa_gerar_vt ).

*-----------------------------------------
*-- selecao dados
*-----------------------------------------
    SELECT *
      INTO @DATA(w_zlest0211)
      FROM zlest0211
        UP TO 1 ROWS
      WHERE vbeln = @i_remessa_dummy.
    ENDSELECT.

    SELECT auart
      INTO @DATA(l_auart)
      FROM vbak
        UP TO 1 ROWS
     WHERE vbeln = @i_vbeln_venda.
    ENDSELECT.

    SELECT shtyp
      INTO @DATA(l_shtyp)
      FROM zsdt0011
        UP TO 1 ROWS
     WHERE auart = @l_auart.
    ENDSELECT.

    SELECT inco1
      INTO @DATA(l_inco1)
      FROM vbkd
        UP TO 1 ROWS
      WHERE vbeln = @i_ov_dummy.
    ENDSELECT.

    SELECT SINGLE vbeln, inco1
      FROM likp INTO @DATA(lwa_likp_sel)
     WHERE vbeln EQ @i_remessa_dummy.

      IF sy-subrc EQ 0 AND lwa_likp_sel-inco1 IS NOT INITIAL.
        l_inco1 =  lwa_likp_sel-inco1.
      ENDIF.

      SELECT route, matnr, werks
        INTO TABLE @DATA(t_vbap)
        FROM vbap
       WHERE vbeln = @i_ov_dummy.

*-----------------------------------------
*-- tratar informacoes
*-----------------------------------------
      READ TABLE t_vbap INTO DATA(w_vbap) INDEX 1.

      IF l_inco1 = 'CIF'.
        SELECT SINGLE *
          FROM a917
          INTO w_a917
         WHERE kappl  = 'F'
           AND kschl  = 'ZSEG'
           AND matnr  = w_vbap-matnr
           AND tdlnr  = w_zlest0211-agente_frete
           AND kfrst  = ''
           AND datbi GE sy-datum.

        IF sy-subrc NE 0.
          l_mensagem = 'Não existe % p/ desc. de Seguro. Mat.:' && w_vbap-matnr.
          zif_remessa_terceiro~set_tab_return( i_type = 'E' i_id = 'SD' i_number = '024' i_message = l_mensagem ).
          t_return = zif_remessa_terceiro~get_tab_return( ).
          zif_remessa_terceiro~set_criar_log( EXPORTING i_vbeln      = i_vbeln_venda
                                                        i_nf_venda   = i_nf_venda
                                                        i_etapa_proc = zif_remessa_terceiro~c_etapa_gerar_vt
                                                        i_commit     = abap_true
                                               CHANGING t_return     = t_return[] ).
          EXIT.
        ENDIF.

        "Identificar se existe valor de seguro e IOF cadastrado, antes da criação do documento de transporte
        SELECT SINGLE *
          FROM a917
          INTO w_a917
         WHERE kappl  = 'F'
           AND kschl  = 'ZIOF'
           AND matnr  = w_vbap-matnr
           AND tdlnr  = w_zlest0211-agente_frete
           AND kfrst  = ''
           AND datbi GE sy-datum.

        IF sy-subrc NE 0.
          l_mensagem = 'Não existe % p/ desc. de IOF Mat.:' && w_vbap-matnr.
          zif_remessa_terceiro~set_tab_return( i_type = 'E' i_id = 'SD' i_number = '024' i_message = l_mensagem ).
          t_return = zif_remessa_terceiro~get_tab_return( ).
          zif_remessa_terceiro~set_criar_log( EXPORTING i_vbeln      = i_vbeln_venda
                                                        i_nf_venda   = i_nf_venda
                                                        i_etapa_proc = zif_remessa_terceiro~c_etapa_gerar_vt
                                                        i_commit     = abap_true
                                               CHANGING t_return     = t_return[] ).
          EXIT.
        ENDIF.
      ENDIF.

      l_lifnr = w_zlest0211-cod_loc_coleta.

      SELECT lifnr, lzone, ktokk
        FROM lfa1
        INTO @DATA(w_lfa1)
          UP TO 1 ROWS
       WHERE lifnr   = @l_lifnr.
      ENDSELECT.

      l_kunnr = w_zlest0211-cod_loc_entrega.

      SELECT shtyp, laufk
        FROM tvtk
        INTO @DATA(w_tvtk)
          UP TO 1 ROWS
       WHERE shtyp = @l_shtyp.
      ENDSELECT.

      SELECT *
        FROM tvro
        INTO @DATA(w_tvro)
          UP TO 1  ROWS
       WHERE route  = @w_vbap-route.
      ENDSELECT.

      CHECK sy-subrc = 0.

      IF w_tvro-traztd GT 24.
        w_tvro-traztd = w_tvro-traztd / 24.
      ENDIF.

*-----------------------------------------
*-- montar header
*-----------------------------------------
      CLEAR st_headerdata.

      st_headerdata-service_agent_id        = w_zlest0211-agente_frete.
      st_headerdata-service_level           = '1'.
      st_headerdata-shipping_type           = '01'.
      st_headerdata-status_plan             = 'X'.
      st_headerdata-status_checkin          = 'X'.
      st_headerdata-status_load_start       = 'X'.
      st_headerdata-special_procedure_id 	  = '0001'.
      st_headerdata-shpmnt_cost_rel         = 'X'.
      st_headerdata-shipment_type           = l_shtyp.
      st_headerdata-trans_plan_pt           = w_vbap-werks.
      st_headerdata-shipment_route          = w_vbap-route.
      st_headerdata-distance                = w_tvro-distz.
      st_headerdata-distance_unit           = w_tvro-medst.
      st_headerdata-time_travel             =	w_tvro-traztd.
      st_headerdata-time_unit               =	'H'.
*   st_headerdata-text_1                  =	w_zlest0211-placa_cav.
*   st_headerdata-text_2                  =	w_zlest0211-placa_car1.
*   st_headerdata-text_3                  =	w_zlest0211-placa_car2.
*   st_headerdata-text_4                  =	w_zlest0211-placa_car3.

*-----------------------------------------
*-- etapa
*-----------------------------------------
      CLEAR st_stagedata.
      REFRESH t_stagedata.

      st_stagedata-stage_cat      = '1'.
      st_stagedata-stage_seq      = '0001'.
      st_stagedata-shipping_type  =  '01'.
      st_stagedata-service_agent  = w_zlest0211-agente_frete.

      IF w_lfa1-ktokk = 'ZFIC'.
        st_stagedata-org_shipp_dpmnt = w_lfa1-lifnr+6(4).
      ELSE.
        st_stagedata-org_suppl       = w_lfa1-lifnr.
      ENDIF.

      st_stagedata-leg_indicator = w_tvtk-laufk. "Código de Percurso

      "Se Código de Percurso  igual a 1:  Percurso preliminar
      IF w_tvtk-laufk = 1.
        SELECT SINGLE knote
          FROM tvkn
          INTO @DATA(l_knote)
         WHERE kunnr   = @l_kunnr.

        IF sy-subrc = 0.
          st_stagedata-dest_point   = l_knote.
        ELSE.
          st_stagedata-dest_point   = l_kunnr.
        ENDIF.

        "Se Código de Percurso  igual a 4:  Percurso direto
      ELSEIF w_tvtk-laufk = 4.
        IF st_headerdata-shipment_type  =  'Z004'.
          SELECT SINGLE stcd1
            FROM kna1
            INTO @DATA(v_stcd1)
           WHERE kunnr = @l_kunnr.

          SELECT SINGLE lifnr
            FROM lfa1
            INTO @DATA(v_lifnr)
           WHERE stcd1 = @v_stcd1.

          st_stagedata-dest_suppl  = v_lifnr.

        ELSE.
          st_stagedata-dest_cust   = l_kunnr. "Local de entrega (V_KUNNR)
        ENDIF.
      ENDIF.

      APPEND st_stagedata TO t_stagedata.

*-----------------------------------------
*-- itens
*-----------------------------------------
      CLEAR st_itemdata.
      REFRESH t_itemdata.

      st_itemdata-delivery    = i_remessa_dummy.
      st_itemdata-itenerary   = '000010'.
      APPEND st_itemdata     TO t_itemdata.

*-----------------------------------------
*-- Gera o Transporte
*-----------------------------------------
"*---> 12/07/2023 - Migração S4 - LO --> Material não foi utilizado
      CALL FUNCTION 'BAPI_SHIPMENT_CREATE'"#EC CI_USAGE_OK[2438131]
        EXPORTING
          headerdata = st_headerdata
        IMPORTING
          transport  = l_tknum
        TABLES
          itemdata   = t_itemdata
          stagedata  = t_stagedata
          return     = t_return_vt.

      IF l_tknum IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        zif_remessa_terceiro~set_criar_log( EXPORTING i_vbeln      = i_vbeln_venda
                                                      i_nf_venda   = i_nf_venda
                                                      i_etapa_proc = zif_remessa_terceiro~c_etapa_gerar_vt
                                                      i_doc_gerado = l_tknum
                                             CHANGING t_return     = t_return_vt[] ).

        RAISE EXCEPTION TYPE zcx_remessa_terceiro
          EXPORTING
            textid = VALUE #( msgid  = zcx_remessa_terceiro=>zcx_erro_criar_transporte-msgid
                              msgno  = zcx_remessa_terceiro=>zcx_erro_criar_transporte-msgno
                              attr1  = CONV #( i_remessa_dummy ) )
            msgty  = 'E'
            msgid  = zcx_remessa_terceiro=>zcx_erro_criar_transporte-msgid
            msgno  = zcx_remessa_terceiro=>zcx_erro_criar_transporte-msgno
            msgv1  = CONV #( i_remessa_dummy ).

      ELSE.
        zif_remessa_terceiro~set_criar_log( EXPORTING i_vbeln      = i_vbeln_venda
                                                      i_nf_venda   = i_nf_venda
                                                      i_etapa_proc = zif_remessa_terceiro~c_etapa_gerar_vt
                                                      i_doc_gerado = l_tknum
                                                      i_commit     = abap_false
                                             CHANGING t_return     = t_return_vt[] ).

*-----------------------------
*---- Atualiza documento
*-----------------------------
        UPDATE zlest0211 SET doc_transp = l_tknum
                             st_proc    = '04'
                       WHERE vbeln      = i_remessa_dummy.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_on.

        CLEAR : st_headerdataaction, st_headerdata2.
        REFRESH: t_return_vt.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = l_tknum
          IMPORTING
            output = st_headerdata2-shipment_num.

        l_tknum                              = st_headerdata2-shipment_num.
        st_headerdata2-status_load_end       = 'X'.
        st_headerdataaction-status_load_end  = 'C'.

        CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
          EXPORTING
            headerdata       = st_headerdata2
            headerdataaction = st_headerdataaction
          TABLES
            return           = t_return_vt.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        IF l_inco1 = 'CIF'.
          CALL FUNCTION 'Z_LES_VERIFICA_PED_ADM'
            EXPORTING
              p_tknum      = st_headerdata2-shipment_num
            EXCEPTIONS
              adiantamento = 1
              pedagio      = 2
              OTHERS       = 3.
        ENDIF.

        IF sy-subrc IS NOT INITIAL.

          DATA(lva_msg_id) = sy-msgid.
          DATA(lva_msg_no) = sy-msgno.

          IF lva_msg_id IS INITIAL .
            lva_msg_id = 'SD'.
          ENDIF.

          IF lva_msg_no IS INITIAL .
            lva_msg_id = '024'.
          ENDIF.

          MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                              INTO l_mensagem.
          zif_remessa_terceiro~set_tab_return( i_type = 'E' i_id = CONV #( lva_msg_id ) i_number = CONV #( lva_msg_no ) i_message = l_mensagem ).
          l_mensagem = 'Documento VT será estornado:' && l_tknum.
          zif_remessa_terceiro~set_tab_return( i_type = 'E' i_id = CONV #( lva_msg_id ) i_number = CONV #( lva_msg_no ) i_message = l_mensagem ).

          t_return = zif_remessa_terceiro~get_tab_return( ).
          zif_remessa_terceiro~set_criar_log( EXPORTING i_vbeln      = i_vbeln_venda
                                                        i_nf_venda   = i_nf_venda
                                                        i_etapa_proc = zif_remessa_terceiro~c_etapa_gerar_vt
                                                        i_commit     = abap_true
                                               CHANGING t_return     = t_return[] ).

*-----------------------------
*------ Estorna VT
*-----------------------------
          zif_remessa_terceiro~set_elimina_vt( EXPORTING i_tknum         = l_tknum
                                                         i_vbeln_venda   = i_vbeln_venda
                                                         i_nf_venda      = i_nf_venda
                                                         i_remessa_dummy = i_remessa_dummy ).
          FREE: l_tknum.
        ELSE.
          st_headerdata2-status_compl             = 'X'.
          st_headerdata2-status_shpmnt_start      = 'X'.
          st_headerdataaction-status_compl        = 'C'.
          st_headerdataaction-status_shpmnt_start = 'C'.

          CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
            EXPORTING
              headerdata       = st_headerdata2
              headerdataaction = st_headerdataaction
            TABLES
              return           = t_return_vt.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
        ENDIF.
      ENDIF.

*--------------------------------
*-- documento gerado
*--------------------------------
      e_doc_transp = l_tknum.

    ENDMETHOD.


  METHOD zif_remessa_terceiro~set_elimina_transporte.

    DELETE FROM zlest0211 WHERE vbeln = i_remessa_dummy.
    DELETE FROM zlest0212 WHERE vbeln = i_remessa_dummy.
    DELETE FROM zlest0213 WHERE vbeln = i_remessa_dummy.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD zif_remessa_terceiro~set_elimina_vt.

    DATA: st_headerdata       TYPE bapishipmentheader,
          st_stagedata        TYPE bapishipmentstage,
          t_stagedata         TYPE TABLE OF bapishipmentstage,
          t_itemdata          TYPE TABLE OF bapishipmentitem,
          st_itemdata         TYPE bapishipmentitem,
          st_headerdata2      TYPE bapishipmentheader,
          st_headerdataaction TYPE bapishipmentheaderaction,
          t_itemdataaction    TYPE TABLE OF bapishipmentitemaction,
          w_itemdataaction    TYPE bapishipmentitemaction,
          t_return_vt         TYPE TABLE OF bapiret2.

    FREE: st_headerdataaction, st_headerdata, t_itemdataaction, st_headerdata2,
          t_itemdataaction,    t_return_vt,
          t_itemdata.

    CLEAR st_itemdata.
    st_itemdata-delivery   = i_remessa_dummy.
    st_itemdata-itenerary  = '0010'.
    APPEND st_itemdata    TO t_itemdata.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = i_tknum
      IMPORTING
        output = st_headerdata2-shipment_num.

    st_headerdataaction-shipment_num     = 'D'.
    st_headerdataaction-service_agent_id = 'D'.

    LOOP AT t_itemdata      INTO st_itemdata.
      CLEAR: w_itemdataaction.
      MOVE: 'D'               TO w_itemdataaction-delivery,
            'D'               TO w_itemdataaction-itenerary.
      APPEND w_itemdataaction TO t_itemdataaction.
    ENDLOOP.
*
    CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
      EXPORTING
        headerdata       = st_headerdata2
        headerdataaction = st_headerdataaction
      TABLES
        itemdata         = t_itemdata
        itemdataaction   = t_itemdataaction
        return           = t_return_vt.

    IF i_estorno = abap_true.
      zif_remessa_terceiro~set_criar_log( EXPORTING i_vbeln      = i_vbeln_venda
                                                    i_nf_venda   = i_nf_venda
                                                    i_etapa_proc = zif_remessa_terceiro~c_etapa_estorno_vt
                                                    i_commit     = abap_false
                                           CHANGING t_return     = t_return_vt[] ).
*-----------------------------
*---- Atualiza documento
*-----------------------------
      UPDATE zlest0211 SET doc_transp = abap_off
                           st_proc    = '03'
                     WHERE vbeln      = i_remessa_dummy.

    ELSE.
      zif_remessa_terceiro~set_criar_log( EXPORTING i_vbeln      = i_vbeln_venda
                                                    i_nf_venda   = i_nf_venda
                                                    i_etapa_proc = zif_remessa_terceiro~c_etapa_gerar_vt
                                                    i_commit     = abap_false
                                           CHANGING t_return     = t_return_vt[] ).
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

  ENDMETHOD.


  METHOD zif_remessa_terceiro~set_estorna_cte.

    r_instancia = me.

*------------------------------------
*-- checa doctos criados
*------------------------------------
    SELECT *
      FROM zlest0211
      INTO @DATA(w_zlest0211)
        UP TO 1 ROWS
     WHERE vbeln = @i_remessa_dummy.
    ENDSELECT.

    CHECK sy-subrc = 0.

*------------------------------------
*-- estorno fatura servico
*------------------------------------
    IF w_zlest0211-fatura_frete IS NOT INITIAL.
      zif_remessa_terceiro~set_estorno_fatura_serv( i_vbeln_venda   = i_vbeln_venda
                                                    i_nf_venda      = i_nf_venda
                                                    i_remessa_dummy = i_remessa_dummy ).
    ENDIF.

*------------------------------------
*-- estorno OV frete
*------------------------------------
    IF w_zlest0211-ov_frete IS NOT INITIAL.
      zif_remessa_terceiro~set_estorno_ordem_serv(  i_vbeln_venda   = i_vbeln_venda
                                                    i_nf_venda      = i_nf_venda
                                                    i_remessa_dummy = i_remessa_dummy ).
    ENDIF.

*------------------------------------
*-- estorno doc custo
*------------------------------------
    IF w_zlest0211-fknum IS NOT INITIAL.
      zif_remessa_terceiro~set_estorno_doc_custo(   i_vbeln_venda   = i_vbeln_venda
                                                    i_nf_venda      = i_nf_venda
                                                    i_remessa_dummy = i_remessa_dummy ).
    ENDIF.

*------------------------------------
*-- estorno doc transporte
*------------------------------------
    IF w_zlest0211-doc_transp IS NOT INITIAL.
      zif_remessa_terceiro~set_elimina_vt(        i_tknum         = w_zlest0211-doc_transp
                                                  i_vbeln_venda   = i_vbeln_venda
                                                  i_nf_venda      = i_nf_venda
                                                  i_estorno       = abap_true
                                                  i_remessa_dummy = i_remessa_dummy ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_remessa_terceiro~set_estorno_doc_custo.

    DATA: l_tp_frete TYPE zde_tp_frete,
          l_fknum    TYPE char40,
          l_vdata    TYPE char40,
          l_mode     TYPE char1,
          t_msg      TYPE TABLE OF bdcmsgcoll,
          w_msg      TYPE bdcmsgcoll,
          t_return   TYPE TABLE OF bapiret2,
          w_return   TYPE bapiret2.

    FREE: l_tp_frete,
          t_return.

*------------------------------------
*-- checa doctos criados
*------------------------------------
    SELECT *
      FROM zlest0211
      INTO @DATA(w_zlest0211)
        UP TO 1 ROWS
     WHERE vbeln = @i_remessa_dummy.
    ENDSELECT.

    CHECK sy-subrc = 0.

    CHECK w_zlest0211-fknum IS NOT INITIAL.

    l_fknum = w_zlest0211-fknum.
    l_vdata = sy-datum+6(2) && '.' && sy-datum+4(2) && '.' && sy-datum+0(4).
    l_mode  = 'N'.

*--------------------------------------
*-- cif / ctp
*--------------------------------------
    SELECT ktokk
      INTO @DATA(l_ktokk)
      FROM lfa1
        UP TO 1 ROWS
      WHERE lifnr = @w_zlest0211-agente_frete.
    ENDSELECT.

    IF l_ktokk IS NOT INITIAL.
      IF l_ktokk = 'ZFIC'.
        l_tp_frete   = 'CIF'.
      ELSE.
        l_tp_frete   = 'CPT'.
      ENDIF.
    ENDIF.

*--------------------------------------
*-- log de proc
*--------------------------------------
    zif_remessa_terceiro~set_free_return( ).
    zif_remessa_terceiro~set_free_log_proc( EXPORTING i_vbeln      = i_vbeln_venda
                                                      i_nf_venda   = i_nf_venda
                                                      i_etapa_proc = zif_remessa_terceiro~c_etapa_estorno_doc_custo ).

*--------------------------------------
*-- monta tela
*--------------------------------------
    zif_remessa_terceiro~set_free_bdc_data( ).

    IF l_tp_frete <> 'CPT'.
      zif_remessa_terceiro~set_bdc_data( i_prog = 'SAPMV54A' i_dynpro = '0020' i_start = 'X' i_fnam = ''               i_fval = '' ).
      zif_remessa_terceiro~set_bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_CURSOR'     i_fval = 'VFKK-FKNUM' ).
      zif_remessa_terceiro~set_bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_OKCODE'     i_fval = '=UEBP' ).
      zif_remessa_terceiro~set_bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'VFKK-FKNUM'     i_fval = l_fknum ).

      zif_remessa_terceiro~set_bdc_data( i_prog = 'SAPMV54A' i_dynpro = '0030' i_start = 'X' i_fnam = ''               i_fval = '' ).
      zif_remessa_terceiro~set_bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_CURSOR'     i_fval = 'VFKP-FKPOS(01)' ).
      zif_remessa_terceiro~set_bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_OKCODE'     i_fval = '=PDET' ).

      zif_remessa_terceiro~set_bdc_data( i_prog = 'SAPMV54A' i_dynpro = '0040' i_start = 'X' i_fnam = ''               i_fval = '' ).
      zif_remessa_terceiro~set_bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_CURSOR'     i_fval = 'VFKP-POSTX' ).
      zif_remessa_terceiro~set_bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_OKCODE'     i_fval = '=PABR' ).

      zif_remessa_terceiro~set_bdc_data( i_prog = 'SAPMV54A' i_dynpro = '0040' i_start = 'X' i_fnam = ''               i_fval = '' ).
      zif_remessa_terceiro~set_bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_OKCODE'     i_fval = '=SICH' ).
      zif_remessa_terceiro~set_bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'VFKPD-SLSTOR'   i_fval = 'X' ).
    ELSE.
      zif_remessa_terceiro~set_bdc_data( i_prog = 'SAPMV54A' i_dynpro = '0020' i_start = 'X' i_fnam = ''               i_fval = '' ).
      zif_remessa_terceiro~set_bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_CURSOR'     i_fval = 'VFKK-FKNUM' ).
      zif_remessa_terceiro~set_bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_OKCODE'     i_fval = '=UEBP' ).
      zif_remessa_terceiro~set_bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'VFKK-FKNUM'     i_fval = l_fknum ).

      zif_remessa_terceiro~set_bdc_data( i_prog = 'SAPMV54A' i_dynpro = '0030' i_start = 'X' i_fnam = ''               i_fval = '' ).
      zif_remessa_terceiro~set_bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_CURSOR'     i_fval = 'VFKP-FKPOS(01)' ).
      zif_remessa_terceiro~set_bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_OKCODE'     i_fval = '=PDET' ).

      zif_remessa_terceiro~set_bdc_data( i_prog = 'SAPMV54A' i_dynpro = '0040' i_start = 'X' i_fnam = ''               i_fval = '' ).
      zif_remessa_terceiro~set_bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_OKCODE'     i_fval = '=PABR' ).

      zif_remessa_terceiro~set_bdc_data( i_prog = 'SAPMV54A' i_dynpro = '0040' i_start = 'X' i_fnam = ''               i_fval = '' ).
      zif_remessa_terceiro~set_bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_OKCODE'     i_fval = '=KLAC' ).
      zif_remessa_terceiro~set_bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'VFKPD-SLSTOR'   i_fval = 'X' ).

      zif_remessa_terceiro~set_bdc_data( i_prog = 'SAPMV54A' i_dynpro = '0040' i_start = 'X' i_fnam = ''               i_fval = '' ).
      zif_remessa_terceiro~set_bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_OKCODE'     i_fval = '/00' ).
      zif_remessa_terceiro~set_bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'VFKPD-STDAT'    i_fval = l_vdata ).

      zif_remessa_terceiro~set_bdc_data( i_prog = 'SAPMV54A' i_dynpro = '0040' i_start = 'X' i_fnam = ''               i_fval = '' ).
      zif_remessa_terceiro~set_bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_OKCODE'     i_fval = '=SICH' ).

    ENDIF.

*--------------------------------------
*-- call vi02
*--------------------------------------
    CALL TRANSACTION 'VI02' USING zif_remessa_terceiro~t_bdcdata
                MODE l_mode
       MESSAGES INTO t_msg.

*------------------------------------
*-- trata mensagen
*------------------------------------
    LOOP AT t_msg      INTO w_msg.
      w_return-type       = w_msg-msgtyp.
      w_return-id         = w_msg-msgid.
      w_return-number     = w_msg-msgnr.
      w_return-message_v1 = w_msg-msgv1.
      w_return-message_v2 = w_msg-msgv2.
      w_return-message_v3 = w_msg-msgv3.
      w_return-message_v4 = w_msg-msgv4.
      APPEND w_return    TO t_return.
    ENDLOOP.

*------------------------------------
*-- monta log
*------------------------------------
    zif_remessa_terceiro~set_criar_log( EXPORTING i_vbeln      = i_vbeln_venda
                                                  i_nf_venda   = i_nf_venda
                                                  i_etapa_proc = zif_remessa_terceiro~c_etapa_estorno_doc_custo
                                                  i_commit     = abap_false
                                         CHANGING t_return     = t_return[] ).

    READ TABLE t_msg INTO w_msg WITH KEY msgtyp = 'E'.

    CHECK sy-subrc <> 0.

    COMMIT WORK.
    WAIT UP TO 5 SECONDS.

*--------------------------------------
*-- elimina VI
*--------------------------------------
    FREE: t_msg,
          t_return.

    zif_remessa_terceiro~set_free_bdc_data( ).

    zif_remessa_terceiro~set_bdc_data( i_prog = 'SAPMV54A' i_dynpro = '0020' i_start = 'X' i_fnam = ''               i_fval = '' ).
    zif_remessa_terceiro~set_bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_CURSOR'     i_fval = 'VFKK-FKNUM' ).
    zif_remessa_terceiro~set_bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_OKCODE'     i_fval = '=UEBP' ).
    zif_remessa_terceiro~set_bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'VFKK-FKNUM'     i_fval = l_fknum ).

    zif_remessa_terceiro~set_bdc_data( i_prog = 'SAPMV54A' i_dynpro = '0030' i_start = 'X' i_fnam = ''               i_fval = '' ).
    zif_remessa_terceiro~set_bdc_data( i_prog = ''         i_dynpro = ''     i_start = ''  i_fnam = 'BDC_OKCODE'     i_fval = '/ELOES' ).

*--------------------------------------
*-- call vi02
*--------------------------------------
    CALL TRANSACTION 'VI02' USING zif_remessa_terceiro~t_bdcdata
                MODE l_mode
       MESSAGES INTO t_msg.

*------------------------------------
*-- trata mensagen
*------------------------------------
    LOOP AT t_msg      INTO w_msg.
      w_return-type       = w_msg-msgtyp.
      w_return-id         = w_msg-msgid.
      w_return-number     = w_msg-msgnr.
      w_return-message_v1 = w_msg-msgv1.
      w_return-message_v2 = w_msg-msgv2.
      w_return-message_v3 = w_msg-msgv3.
      w_return-message_v4 = w_msg-msgv4.
      APPEND w_return    TO t_return.
    ENDLOOP.

*------------------------------------
*-- monta log
*------------------------------------
    zif_remessa_terceiro~set_criar_log( EXPORTING i_vbeln      = i_vbeln_venda
                                                  i_nf_venda   = i_nf_venda
                                                  i_etapa_proc = zif_remessa_terceiro~c_etapa_estorno_doc_custo
                                                  i_commit     = abap_false
                                         CHANGING t_return     = t_return[] ).

    READ TABLE t_msg INTO w_msg WITH KEY msgtyp = 'E'.

    CHECK sy-subrc <> 0.

*-----------------------------
*-- Atualiza documento
*-----------------------------
    UPDATE zlest0211 SET fknum    = abap_off
                         st_proc  = '04'
                   WHERE vbeln    = i_remessa_dummy.

    COMMIT WORK.
    WAIT UP TO 5 SECONDS.

  ENDMETHOD.


  METHOD zif_remessa_terceiro~set_estorno_fatura_serv.

    DATA: l_bukrs   TYPE j_1bnfdoc-bukrs,
          l_docnum  TYPE j_1bnflin-docnum,
          t_return2 TYPE TABLE OF bapireturn1,
          t_return  TYPE TABLE OF bapiret2,
          t_success TYPE TABLE OF bapivbrksuccess.

    FREE: t_return,
          t_return2,
          t_success.

*--------------------------------------
*-- log de proc
*--------------------------------------
    zif_remessa_terceiro~set_free_return( ).
    zif_remessa_terceiro~set_free_log_proc( EXPORTING i_vbeln      = i_vbeln_venda
                                                      i_nf_venda   = i_nf_venda
                                                      i_etapa_proc = zif_remessa_terceiro~c_etapa_estorno_fatura_serv ).

*------------------------------------
*-- checa doctos criados
*------------------------------------
    SELECT *
      FROM zlest0211
      INTO @DATA(w_zlest0211)
        UP TO 1 ROWS
     WHERE vbeln = @i_remessa_dummy.
    ENDSELECT.

    CHECK sy-subrc = 0.

    CHECK w_zlest0211-fatura_frete IS NOT INITIAL.

*------------------------------------
*-- pega NF
*------------------------------------
    SELECT SINGLE j_1bnfdoc~bukrs j_1bnflin~docnum
             FROM j_1bnflin
            INNER JOIN j_1bnfdoc ON j_1bnfdoc~docnum = j_1bnflin~docnum
             INTO (l_bukrs,l_docnum)
            WHERE j_1bnflin~refkey = w_zlest0211-fatura_frete.

*------------------------------------
*-- estorno
*------------------------------------
    CALL FUNCTION 'ZBAPI_BILLINGDOC_CANCEL1'
      EXPORTING
        billingdocument = w_zlest0211-fatura_frete
      TABLES
        return          = t_return2
        success         = t_success.

    MOVE-CORRESPONDING t_return2[]  TO t_return[].

*------------------------------------
*-- monta log
*------------------------------------
    zif_remessa_terceiro~set_criar_log( EXPORTING i_vbeln      = i_vbeln_venda
                                                  i_nf_venda   = i_nf_venda
                                                  i_etapa_proc = zif_remessa_terceiro~c_etapa_estorno_fatura_serv
                                                  i_commit     = abap_false
                                         CHANGING t_return     = t_return[] ).

    CHECK t_success[] IS NOT INITIAL..

*-----------------------------
*-- Atualiza documento
*-----------------------------
    UPDATE zlest0211 SET fatura_frete = abap_off
                         st_proc      = '06'
                   WHERE vbeln        = i_remessa_dummy.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    WAIT UP TO 5 SECONDS.

  ENDMETHOD.


  METHOD zif_remessa_terceiro~set_estorno_ordem_serv.

    DATA: tl_bapiparex      TYPE TABLE OF bapiparex,
          sl_bapiparex      TYPE bapiparex,
          wl_orderheaderin  TYPE bapisdh1,
          wl_orderheaderinx TYPE bapisdh1x,
          wl_bape_vbak      TYPE bape_vbak,
          wl_bape_vbakx     TYPE bape_vbakx,
          t_itemdata        TYPE TABLE OF bapishipmentitem,
          st_itemdata       TYPE bapishipmentitem,
          t_return_vt       TYPE TABLE OF bapiret2,
          w_return_vt       TYPE bapiret2.

*------------------------------------
*-- checa doctos criados
*------------------------------------
    SELECT *
      FROM zlest0211
      INTO @DATA(w_zlest0211)
        UP TO 1 ROWS
     WHERE vbeln = @i_remessa_dummy.
    ENDSELECT.

    CHECK sy-subrc = 0.

    CHECK w_zlest0211-ov_frete IS NOT INITIAL.

*--------------------------------------
*-- log de proc
*--------------------------------------
    zif_remessa_terceiro~set_free_return( ).
    zif_remessa_terceiro~set_free_log_proc( EXPORTING i_vbeln      = i_vbeln_venda
                                                      i_nf_venda   = i_nf_venda
                                                      i_etapa_proc = zif_remessa_terceiro~c_etapa_estorno_ordem_serv ).

*--------------------------------------
*-- monta bapi
*--------------------------------------
    FREE: wl_orderheaderin,wl_orderheaderinx,
          tl_bapiparex.

    wl_bape_vbak-vbeln           = w_zlest0211-ov_frete.
    wl_bape_vbak-tknum           = ''.
    sl_bapiparex-structure       = 'BAPE_VBAK'.
    sl_bapiparex-valuepart1      = wl_bape_vbak.
    APPEND sl_bapiparex         TO tl_bapiparex.

    CLEAR sl_bapiparex.
    wl_bape_vbakx-vbeln          = w_zlest0211-ov_frete.
    wl_bape_vbakx-tknum          = 'X'.
    sl_bapiparex-structure       = 'BAPE_VBAKX'.
    sl_bapiparex-valuepart1      = wl_bape_vbakx.
    APPEND sl_bapiparex         TO tl_bapiparex.

    wl_orderheaderin-bill_block  = '10'.
    wl_orderheaderinx-updateflag = 'U'.
    wl_orderheaderinx-bill_block = 'X'.

*--------------------------------------
*-- executa bapi
*--------------------------------------
"*---> 12/07/2023 - Migração S4 - LO --> Material não foi utilizado
    CALL FUNCTION 'BAPI_SALESORDER_CHANGE'"#EC CI_USAGE_OK[2438131]
      EXPORTING
        salesdocument    = w_zlest0211-ov_frete
        order_header_in  = wl_orderheaderin
        order_header_inx = wl_orderheaderinx
      TABLES
        return           = t_return_vt
        extensionin      = tl_bapiparex.

*--------------------------------------
*-- monta log
*--------------------------------------
    zif_remessa_terceiro~set_criar_log( EXPORTING i_vbeln      = i_vbeln_venda
                                                  i_nf_venda   = i_nf_venda
                                                  i_etapa_proc = zif_remessa_terceiro~c_etapa_estorno_ordem_serv
                                                  i_commit     = abap_false
                                         CHANGING t_return     = t_return_vt[] ).

    READ TABLE t_return_vt INTO w_return_vt WITH KEY type = 'E'.

    CHECK sy-subrc <> 0.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

*-----------------------------
*-- Atualiza documento
*-----------------------------
    UPDATE zlest0211 SET ov_frete = abap_off
                         st_proc  = '05'
                   WHERE vbeln    = i_remessa_dummy.

  ENDMETHOD.


  METHOD zif_remessa_terceiro~set_free_bdc_data.

    FREE: zif_remessa_terceiro~t_bdcdata.

  ENDMETHOD.


  METHOD zif_remessa_terceiro~set_free_log_proc.

    IF i_etapa_proc IS INITIAL.
      DELETE FROM zlest0214 WHERE vbeln      = i_vbeln
                              AND nf_venda   = i_nf_venda
                              AND msgtyp     = zif_remessa_terceiro~c_e.
    ELSE.
      DELETE FROM zlest0214 WHERE vbeln      = i_vbeln
                              AND nf_venda   = i_nf_venda
                              AND etapa_proc = i_etapa_proc.
    ENDIF.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD ZIF_REMESSA_TERCEIRO~SET_FREE_RETURN.

    FREE: zif_remessa_terceiro~at_t_return[].

  ENDMETHOD.


  METHOD zif_remessa_terceiro~set_limpa_erros_log.

    CHECK t_vbeln_venda[] IS NOT INITIAL.

    SELECT *
      FROM zlest0214
      INTO TABLE @DATA(t_0214)
       FOR ALL ENTRIES IN @t_vbeln_venda
     WHERE vbeln      = @t_vbeln_venda-vbeln
       AND nf_venda   = @t_vbeln_venda-nf_venda.

    DELETE t_0214 WHERE msgtyp <> 'E'.

    CHECK t_0214[] IS NOT INITIAL.

    SORT t_0214 BY vbeln etapa_proc.
    DELETE ADJACENT DUPLICATES FROM t_0214
                          COMPARING vbeln etapa_proc.

    LOOP AT t_0214 INTO DATA(w_0214).
      DELETE FROM zlest0214 WHERE vbeln      = w_0214-vbeln
                              AND nf_venda   = w_0214-nf_venda
                              AND etapa_proc = w_0214-etapa_proc.
    ENDLOOP.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD zif_remessa_terceiro~set_ordem_venda.

    DATA: t_return   TYPE TABLE OF bapiret2,
          w_return   TYPE bapiret2,
          l_mensagem TYPE char255.

    r_instancia = me.

    FREE: t_return.

    zif_remessa_terceiro~set_free_return( ).
    zif_remessa_terceiro~set_free_log_proc( EXPORTING i_vbeln      = i_vbeln_princ
                                                      i_nf_venda   = i_nf_venda
                                                      i_etapa_proc = i_etapa_proc ).

    SELECT *
      INTO me->zif_remessa_terceiro~at_vbak
      FROM vbak
        UP TO 1 ROWS
     WHERE vbeln EQ i_vbeln.
    ENDSELECT.

    IF sy-subrc <> 0.

      IF i_vbeln_princ <> i_vbeln.
        l_mensagem = 'OV DUMMY não foi Localizada'.
      ELSE.
        l_mensagem = 'OV não foi Localizada'.
      ENDIF.
*--------------------------------
*---- Criar log
*--------------------------------
      zif_remessa_terceiro~set_tab_return( i_type = 'E' i_id = 'SD' i_number = '024' i_message = l_mensagem ).

      t_return = zif_remessa_terceiro~get_tab_return( ).

      zif_remessa_terceiro~set_criar_log( EXPORTING i_vbeln      = i_vbeln_princ
                                                    i_nf_venda   = i_nf_venda
                                                    i_etapa_proc = i_etapa_proc
                                           CHANGING t_return     = t_return[] ).

      RAISE EXCEPTION TYPE zcx_remessa_terceiro
        EXPORTING
          textid = VALUE #( msgid  = zcx_remessa_terceiro=>zcx_ordem_venda_nao_existe-msgid
                            msgno  = zcx_remessa_terceiro=>zcx_ordem_venda_nao_existe-msgno
                            attr1  = CONV #( i_vbeln ) )
          msgty  = 'E'
          msgid  = zcx_remessa_terceiro=>zcx_ordem_venda_nao_existe-msgid
          msgno  = zcx_remessa_terceiro=>zcx_ordem_venda_nao_existe-msgno
          msgv1  = CONV #( i_vbeln ).
    ENDIF.

    SELECT *
      INTO TABLE me->zif_remessa_terceiro~at_vbap
      FROM vbap
     WHERE vbeln EQ i_vbeln.

    SELECT *
      INTO TABLE me->zif_remessa_terceiro~at_vbkd
      FROM vbkd
     WHERE vbeln EQ i_vbeln.

    SELECT *
      INTO TABLE me->zif_remessa_terceiro~at_vbpa
      FROM vbpa
     WHERE vbeln EQ i_vbeln.

*---> 05/07/2022 - Migração S4 - DG
*    SELECT *
*      INTO TABLE me->zif_remessa_terceiro~at_konv
*      FROM konv
*     WHERE knumv EQ me->zif_remessa_terceiro~at_vbak-knumv.

    SELECT *
      INTO corresponding fields of TABLE @me->zif_remessa_terceiro~at_konv
      FROM v_konv
     WHERE knumv EQ @me->zif_remessa_terceiro~at_vbak-knumv.
*<--- 05/07/2022 - Migração S4 - DG


  ENDMETHOD.


  METHOD zif_remessa_terceiro~set_salva_transporte.

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

    CREATE OBJECT zcl_util.

    SELECT *
      INTO @DATA(w_vbak)
      FROM vbak
        UP TO 1 ROWS
     WHERE vbeln = @i_ov_dummy.
    ENDSELECT.

    SELECT *
      INTO @DATA(w_kna1)
      FROM kna1
        UP TO 1 ROWS
     WHERE kunnr = @w_vbak-kunnr.
    ENDSELECT.

    SELECT *
      INTO TABLE @DATA(t_vbpa)
      FROM vbpa
     WHERE vbeln = @i_ov_dummy.

    SELECT *
      INTO TABLE @DATA(t_vbap)
      FROM vbap
     WHERE vbeln = @i_ov_dummy.

    SELECT *
      INTO TABLE @DATA(t_lips)
      FROM lips
     WHERE vbeln = @i_remessa_dummy.

    SELECT *
      INTO @DATA(w_zlest0210)
      FROM zlest0210
        UP TO 1 ROWS
     WHERE remessa_dummy = @i_remessa_dummy.
    ENDSELECT.

    SELECT *
      INTO @DATA(w_zib_dist_ter)
      FROM zib_nfe_dist_ter
        UP TO 1 ROWS
     WHERE chave_nfe = @w_zlest0210-chave_nf_cta_ordem.
    ENDSELECT.

    SELECT *
      INTO TABLE @DATA(t_zib_dist_itm)
      FROM zib_nfe_dist_itm
     WHERE chave_nfe = @w_zlest0210-chave_nf_cta_ordem.

*----------------------------------------
*-- NF venda
*----------------------------------------
    w_campos_nfe = zcl_util->get_atributos_nfe( w_zlest0210-chave_nf_venda ).
    w_campos_cta = zcl_util->get_atributos_nfe( w_zlest0210-chave_nf_cta_ordem ).

    SELECT docnum
      INTO @DATA(l_docnum)
      FROM j_1bnfe_active
        UP TO 1 ROWS
     WHERE regio   = @w_campos_nfe-regio
       AND nfyear  = @w_campos_nfe-nfyear
       AND nfmonth = @w_campos_nfe-nfmonth
       AND stcd1   = @w_campos_nfe-stcd1
       AND model   = @w_campos_nfe-model
       AND serie   = @w_campos_nfe-serie
       AND nfnum9  = @w_campos_nfe-nfnum9
       AND docnum9 = @w_campos_nfe-docnum9
       AND cdv     = @w_campos_nfe-cdv.
    ENDSELECT.

    SELECT *
      FROM j_1bnfnad
      INTO TABLE @DATA(t_jnad)
     WHERE docnum = @l_docnum.

    SELECT *
      FROM lfa1
      INTO TABLE @DATA(t_lfa1)
       FOR ALL ENTRIES IN @t_jnad
     WHERE lifnr = @t_jnad-parid.

*----------------------------------------
*-- elimina dados antigos
*----------------------------------------
    DELETE FROM zlest0211 WHERE vbeln = i_remessa_dummy.
    DELETE FROM zlest0212 WHERE vbeln = i_remessa_dummy.
    DELETE FROM zlest0213 WHERE vbeln = i_remessa_dummy.

*----------------------------------------
*-- gravar tabela zlest0211
*----------------------------------------
    LOOP AT t_vbap            INTO DATA(w_vbap).

      CLEAR w_zlest0211.

      w_zlest0211-mandt             = sy-mandt.
      w_zlest0211-vbeln             = i_remessa_dummy.
      w_zlest0211-posnr             = w_vbap-posnr.
      w_zlest0211-vbeln_dummy       = w_vbap-vbeln.
      w_zlest0211-posnr_dummy       = w_vbap-posnr.
      w_zlest0211-peso_liq          = w_vbap-ntgew.
      w_zlest0211-agente_frete      = i_ag_frete.

** US - 92467 - Inicio - CBRAND
      w_zlest0211-peso_bruto =  i_p_bruto.
      w_zlest0211-peso_tara  =  i_p_liquido.
** US - 92467 - Fim- CBRAND

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
      w_zlest0211-region            = w_kna1-regio.

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

      w_zlest0211-safra_ordem_car	= i_nr_safra.  "*-CS2024000522-12.09.2024-JT-#152417-inicio
      w_zlest0211-nro_ordem_car   = i_nr_ordem.  "*-CS2024000522-12.09.2024-JT-#152417-inicio
      w_zlest0211-id_ordem        = i_id_ordem.  "*-CS2024000522-12.09.2024-JT-#152417-inicio

      MODIFY zlest0211           FROM w_zlest0211.
    ENDLOOP.

*----------------------------------------
*-- gravar tabela zlest0212
*----------------------------------------
    LOOP AT t_lips               INTO DATA(w_lips).

      CLEAR w_zlest0212.

      w_zlest0212-mandt             = sy-mandt.
      w_zlest0212-vbeln             = w_lips-vbeln.
      w_zlest0212-posnr             = w_lips-posnr.
      w_zlest0212-vbeln_dummy       = i_ov_dummy.
      w_zlest0212-posnr_dummy       = w_lips-posnr.
      w_zlest0212-werks             = w_lips-werks.
      w_zlest0212-lgort             = w_lips-lgort.
      w_zlest0212-charg             = w_lips-charg.
      w_zlest0212-kunnr             = w_kna1-kunnr.
      w_zlest0212-matnr             = w_lips-matnr.
      w_zlest0212-nfnum             = i_nf_remessa.
      w_zlest0212-qtde_rem          = w_lips-brgew.
      w_zlest0212-unidade           = w_lips-gewei.

      MODIFY zlest0212           FROM w_zlest0212.
    ENDLOOP.

*----------------------------------------
*-- gravar tabela zlest0213
*----------------------------------------
    READ TABLE t_vbap INTO w_vbap INDEX 1.

    LOOP AT t_zib_dist_itm       INTO DATA(w_zib_dist_itm).

      CLEAR: w_zlest0213,
             w_jnad,
             w_lfa1.

      READ TABLE t_jnad INTO w_jnad WITH KEY docnum = l_docnum
                                             parvw  = 'PC'.
      READ TABLE t_lfa1 INTO w_lfa1 WITH KEY lifnr  = w_jnad-parid.

      w_zlest0213-mandt             = sy-mandt.
      w_zlest0213-vbeln             = i_remessa_dummy.
      w_zlest0213-posnr             = w_vbap-posnr.

      IF w_zib_dist_ter-model = '55'.
        w_zlest0213-nfe             = abap_true.
      ENDIF.

      IF w_jnad IS NOT INITIAL.
        w_zlest0213-cliente         = w_jnad-parid.
      ELSE.
        w_zlest0213-cliente         = w_kna1-kunnr.
      ENDIF.

      w_zlest0213-vbeln_dummy       = i_ov_dummy.
      w_zlest0213-posnr_dummy       = w_vbap-posnr.
      w_zlest0213-werks             = w_vbap-werks.
      w_zlest0213-lgort             = w_vbap-lgort.
      w_zlest0213-charg             = w_vbap-charg.
      w_zlest0213-material          = w_vbap-matnr.
      w_zlest0213-modelo            = w_zib_dist_ter-model.
      w_zlest0213-serie             = w_zib_dist_ter-serie.
      w_zlest0213-numero            = w_zib_dist_ter-numero.
      w_zlest0213-dtemissao         = w_zib_dist_ter-dt_emissao.
      w_zlest0213-vl_bc             = w_zib_dist_ter-vl_icms_base.
      w_zlest0213-vl_icms           = w_zib_dist_ter-vl_icms_total.
      w_zlest0213-vl_bc_st          = w_zib_dist_ter-vl_icms_st_base.
      w_zlest0213-vl_st             = w_zib_dist_ter-vl_icms_st_total.
      w_zlest0213-vl_produtos       = w_zib_dist_ter-vl_produtos.
      w_zlest0213-vl_nota_fiscal    = w_zib_dist_ter-vl_total.
      w_zlest0213-quantidade        = w_zib_dist_itm-prod_qtd_comerci.
      w_zlest0213-unidade           = w_zib_dist_itm-prod_und_comerci.
      w_zlest0213-cfop              = w_zib_dist_itm-prod_cfop && 'AA'.
      w_zlest0213-chave             = w_zlest0210-chave_nf_cta_ordem.
      w_zlest0213-docnum9           = w_campos_cta-docnum9.
      w_zlest0213-cdv               = w_campos_cta-cdv.
      w_zlest0213-name1             = w_lfa1-name1.
      w_zlest0213-stcd1             = w_lfa1-stcd1.
      w_zlest0213-stcd2             = w_lfa1-stcd2.
      w_zlest0213-partyp            = 'V'.

      IF w_zlest0213-unidade = 'TL' OR w_zlest0213-unidade = 'TON'.
        w_zlest0213-unidade         = 'TO'.
      ENDIF.

      MODIFY zlest0213           FROM w_zlest0213.
    ENDLOOP.

    COMMIT WORK AND WAIT.

*----------------------------------------
*-- calcular frete
*----------------------------------------
    TRY.
        l_vlr_frete = zcl_remessa_terceiro=>zif_remessa_terceiro~set_calcula_frete(
                         EXPORTING i_vbeln_venda    = i_vbeln_venda
                                   i_remessa_dummy  = i_remessa_dummy
                                   i_nf_venda       = i_nf_venda
                                   i_agente_frete   = i_ag_frete
                         IMPORTING e_unid_cond      = l_unid_cond ).
      CATCH zcx_remessa_terceiro INTO DATA(ex_zcx_remessa_terceiro).
        EXIT.
    ENDTRY.

*----------------------------------------
*-- atualiza frete
*----------------------------------------
    UPDATE zlest0211 SET vlr_frete = l_vlr_frete
                         kbetr     = l_vlr_frete
                         unid_cond = l_unid_cond
                   WHERE vbeln     = i_remessa_dummy.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD zif_remessa_terceiro~set_salva_vinculacao.

    DATA: w_zlest0210     TYPE zlest0210,
          w_zlest0210_old TYPE zlest0210,
          l_ov_dummy      TYPE vbeln_va,
          l_nf_venda      TYPE j_1bnfnum9,
          t_return        TYPE tb_bapiret2,
          w_return        TYPE bapiret2,
          w_campos_nfe    TYPE zde_campos_nfe,
          zcl_util        TYPE REF TO zcl_util.

    CREATE OBJECT zcl_util.

    FREE: t_return,
          w_zlest0210,
          w_zlest0210_old.

*--------------------------
*-- registro atual
*--------------------------
    SELECT *
      FROM zlest0210
      INTO w_zlest0210_old
        UP TO 1 ROWS
     WHERE chave_nf_venda = i_chave_nf_venda.
    ENDSELECT.

*--------------------------
*-- procura remessa dummy
*--------------------------
    SELECT vbeln
      INTO @DATA(l_vbeln)
      FROM vbfa
        UP TO 1 ROWS
     WHERE vbelv   = @i_vbeln_venda
       AND vbtyp_n = 'C'.
    ENDSELECT.

    IF sy-subrc = 0.
      SELECT vbeln
        INTO @DATA(l_remessa_dummy)
        FROM vbfa
          UP TO 1 ROWS
       WHERE vbelv   = @l_vbeln
         AND vbtyp_n = 'J'.
      ENDSELECT.
    ENDIF.

*--------------------------
*-- procura OV dummy
*--------------------------
    l_ov_dummy                      = zcl_remessa_terceiro=>zif_remessa_terceiro~get_status_ov_dummy( i_vbeln          = i_vbeln_venda
                                                                                                      i_chave_nf_venda = i_chave_nf_venda ).

*------------------------------
*-- checa ov DUMMY
*------------------------------
    SELECT vbeln
      INTO @DATA(l_vbeln_dummy)
      FROM vbak
        UP TO 1 ROWS
     WHERE vbeln = @l_ov_dummy.
    ENDSELECT.

    w_zlest0210-mandt               = sy-mandt.
    w_zlest0210-chave_nf_venda      = i_chave_nf_venda.
    w_zlest0210-chave_nf_cta_ordem  = i_chave_nf_cta_ordem.
    w_zlest0210-vbeln_venda         = i_vbeln_venda.
    w_zlest0210-ov_dummy            = l_vbeln_dummy.
    w_zlest0210-remessa_dummy       = w_zlest0210_old-remessa_dummy.

    MODIFY zlest0210             FROM w_zlest0210.

    COMMIT WORK AND WAIT.

*------------------------------
*-- recuperar nf venda
*------------------------------
    w_campos_nfe = zcl_util->get_atributos_nfe( i_chave_nf_venda ).
    l_nf_venda   = w_campos_nfe-nfnum9.

*------------------------------
*-- copiar log de processaento OV dummy
*------------------------------
    CHECK l_vbeln_dummy IS NOT INITIAL.

    SELECT *
      FROM zlest0214
      INTO @DATA(w_0214x)
        UP TO 1 ROWS
     WHERE vbeln      = @i_vbeln_venda
       AND nf_venda   = @l_nf_venda
       AND etapa_proc = @zif_remessa_terceiro~c_etapa_criar_ov_dummy.
    ENDSELECT.

    CHECK sy-subrc <> 0.

    SELECT *
      FROM zlest0214
      INTO TABLE @DATA(t_0214)
     WHERE vbeln      = @i_vbeln_venda
       AND etapa_proc = @zif_remessa_terceiro~c_etapa_criar_ov_dummy.

    DELETE t_0214 WHERE doc_gerado = abap_off.

    SORT t_0214 BY vbeln nf_venda msgtyp msgid msgnr.
    DELETE ADJACENT DUPLICATES FROM t_0214
                        COMPARING vbeln nf_venda msgtyp msgid msgnr.

    CHECK t_0214[] IS NOT INITIAL.

    LOOP AT t_0214     INTO DATA(w_0214).
      w_return-type       = w_0214-msgtyp.
      w_return-id         = w_0214-msgid.
      w_return-number     = w_0214-msgnr.
      w_return-message_v1 = w_0214-msgv1.
      APPEND w_return    TO t_return.
    ENDLOOP.

    zif_remessa_terceiro~set_criar_log( EXPORTING i_vbeln      = i_vbeln_venda
                                                  i_nf_venda   = l_nf_venda
                                                  i_etapa_proc = zif_remessa_terceiro~c_etapa_criar_ov_dummy
                                                  i_doc_gerado = l_vbeln_dummy
                                                  i_commit     = abap_true
                                         CHANGING t_return     = t_return[] ).

  ENDMETHOD.


  METHOD zif_remessa_terceiro~set_tab_return.

    DATA: w_return  TYPE bapiret2.

    w_return-type        = i_type.   "zif_remessa_terceiro~c_e.
    w_return-id          = i_id.     "zif_remessa_terceiro~c_sd.
    w_return-number      = i_number. "zif_remessa_terceiro~c_024.
    w_return-message_v1  = i_message.
    APPEND w_return     TO zif_remessa_terceiro~at_t_return.

  ENDMETHOD.


  METHOD zif_remessa_terceiro~get_agente_frete.

    FREE: e_placa, e_vlr_unit_frete, e_chave_cte.

    SELECT SINGLE vbkd~inco1
      INTO @DATA(_inco1)
      FROM vbak
     INNER JOIN vbkd ON vbkd~vbeln = vbak~vbeln
     WHERE vbak~vbeln  = @i_vbeln.

    CHECK sy-subrc = 0 AND _inco1 = 'CPT'.

    SELECT SINGLE cd_chave_cte
      INTO @DATA(_cd_chave_cte)
      FROM zib_cte_dist_n55
     WHERE n55_chave_acesso = @i_chave_nfe.

    CHECK sy-subrc = 0.

    SELECT SINGLE p_emissor, valor_prestacao, qt_carga_cte
      INTO @DATA(_cte_dist_ter)
      FROM zib_cte_dist_ter
     WHERE cd_chave_cte = @_cd_chave_cte.

    CHECK sy-subrc = 0.

    SELECT SINGLE ds_texto
      INTO @DATA(_ds_texto)
      FROM zib_cte_dist_cpl
     WHERE cd_chave_cte = @_cd_chave_cte
       AND ds_campo     = 'PLACA'.

    IF sy-subrc <> 0.
      CLEAR _ds_texto.
    ENDIF.

    e_agente_frete   = COND #( WHEN _cte_dist_ter-p_emissor IS NOT INITIAL THEN _cte_dist_ter-p_emissor
                                                                           ELSE e_agente_frete ).
    e_placa          = _ds_texto.
    e_vlr_unit_frete = ( _cte_dist_ter-valor_prestacao / _cte_dist_ter-qt_carga_cte ) * 1000.
    e_chave_cte      = _cd_chave_cte.

  ENDMETHOD.
ENDCLASS.
