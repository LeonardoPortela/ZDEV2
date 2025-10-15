*&---------------------------------------------------------------------*
*& Include          ZSDR0197_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_seleciona_dados
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_seleciona_dados .

  DATA: lv_referencia TYPE zsdt0066-referencia.

  o_form_lote->busca_instrucoes( EXPORTING background = abap_true
                                 IMPORTING referencia = lv_referencia
                                 CHANGING  t_instrucoes = t_instrucao  ).


ENDFORM.


*&---------------------------------------------------------------------*
*& Form f_seleciona_dados
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_cria_form_lote.

  TYPES:
    BEGIN OF ty_0066.
  TYPES: icon(4)       TYPE c.
  TYPES: status_trace(4) TYPE c.
         INCLUDE TYPE zsdt0066.
  TYPES: color(4)        TYPE c,
         werks_desc      TYPE name1,
         matnr_desc      TYPE maktx,
         terminal_desc   TYPE name1,
         ponto_c_desc    TYPE name1,
         lentrega_desc   TYPE name1,
         kunnr_desc      TYPE name1,
         btgew           TYPE gsgew.
  TYPES END OF ty_0066.

  DATA: lv_seq         TYPE zsdt0045-zseq_inst,
        lw_header      TYPE ty_0066,
        lw_zsdt0328    TYPE zsdt0328,
        lt_zsdt0328    TYPE TABLE OF zsdt0328,
        lt_0066        TYPE TABLE OF ty_0066,
        lv_txt         TYPE char30,
        lv_msg         TYPE char100,
        lv_bloqueio    TYPE c,
        lv_desbloqueio TYPE c.

  LOOP AT t_instrucao ASSIGNING FIELD-SYMBOL(<fs_instrucao>).

    CLEAR: lw_header,
           lw_zsdt0328,
           lv_msg,
           lv_bloqueio,
           lv_desbloqueio.

    FREE: lt_zsdt0328,
          lt_0066.

    lw_header-nro_sol_ov = <fs_instrucao>-objek.

    o_form_lote->bloqueia_nro_solicitacao( EXPORTING nro_solicitacao =  lw_header-nro_sol_ov
                                           IMPORTING sucesso = lv_bloqueio ).

    IF lv_bloqueio EQ  abap_false.

      MESSAGE s836(sd) WITH 'Solicitação' <fs_instrucao>-objek 'Bloqueada'.

      CONTINUE.
    ENDIF.

    o_form_lote->get_0045( EXPORTING seq    = <fs_instrucao>-seq
                                     referencia = <fs_instrucao>-referencia
                            CHANGING header = lw_header ).

    lw_header-volum = <fs_instrucao>-quantidade_disp.
    lw_header-zmeng = lw_header-volum * 250.
    lw_header-nro_sol_ov = <fs_instrucao>-objek.
    lw_header-INCO1 = <fs_instrucao>-INCO1. "Ajuste para trazer Incoterms #BG 02.09.2025

    TRY .
        lw_header-dmbtr = zcl_preco=>zif_preco~get_preco_pauta( i_regio = CONV #( <fs_instrucao>-region )
                                                                i_matnr = CONV #( <fs_instrucao>-matnr )
                                                                i_inco1 = CONV #( <fs_instrucao>-inco1 )
                                                                i_last  = abap_true ).
      CATCH zcx_preco INTO DATA(lo_preco).
        CONCATENATE 'Para sequência de instrução'  <fs_instrucao>-seq 'erro:' INTO lv_msg.
        MESSAGE s836(sd) WITH lv_msg lo_preco->get_text( ).
        CONTINUE.
    ENDTRY.

    lw_header-pmein    = 'KG'.
    lw_header-libra_to = <fs_instrucao>-dmbtr  * 100.

    o_form_lote->complementa_dados_header( CHANGING header = lw_header  ).


    lw_header-lgort         = 'ALGD'.
    lw_header-waerk         = 'BRL'.
    lw_header-classificacao = 'R'.
    lw_header-vlrtot        = lw_header-volum * lw_header-zmeng.
*
    MOVE-CORRESPONDING <fs_instrucao> TO lw_zsdt0328.
    MOVE <fs_instrucao>-charg        TO lw_zsdt0328-charg_ori.
    MOVE <fs_instrucao>-seq          TO lw_zsdt0328-zseq_inst.
    MOVE lw_header-nro_sol_ov        TO lw_zsdt0328-nro_sol_ov.
    MOVE lw_header-volum             TO lw_zsdt0328-quantidade.

    APPEND lw_zsdt0328               TO lt_zsdt0328.

    o_form_lote->add_novo_lote( EXPORTING background = abap_true
                                IMPORTING texto_bt_dinamico = lv_txt
                                CHANGING header = lw_header
                                         t_0066 = lt_0066 ).

    TRY .

        o_form_lote->save( EXPORTING background = abap_true
                                     nro_solicitacao = lw_zsdt0328-nro_sol_ov
                           CHANGING header = lw_header
                                    t_0066 = lt_0066
                                    t_0328 = lt_zsdt0328 ).

      CATCH zcl_cx_excecoes_cad_form_lote INTO DATA(lo_form_lote).
        CONCATENATE 'Para sequência de instrução'  <fs_instrucao>-seq 'erro:' INTO lv_msg.
        MESSAGE s836(sd) WITH lv_msg lo_form_lote->get_text( ).
        CONTINUE.
    ENDTRY.

    o_form_lote->liberar_lote( EXPORTING background      = abap_true
                                         nro_solicitacao = lw_header-nro_sol_ov
                               IMPORTING mensagem        = lv_msg
                               CHANGING  t_0066          = lt_0066 ).

    IF lv_msg IS NOT INITIAL.
      MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

    o_form_lote->desbloqueia_nro_solicitacao( EXPORTING nro_solicitacao =  lw_header-nro_sol_ov
                                              IMPORTING sucesso = lv_desbloqueio ).

  ENDLOOP.

ENDFORM.
