*&---------------------------------------------------------------------*
*&  Include           ZXV56U16
*&---------------------------------------------------------------------*

DATA : vl_vbeln            TYPE vbak-vbeln,
       wa_filial           TYPE j_1bbranch,
       wa_real             TYPE zsdt_depara_cen,
       wa_xvttk            TYPE vttkvb,
       wa_vttk             TYPE vttk,
       sem_pv              TYPE char01,
       wa_tvepz            TYPE tvepz,
       wa_tvep             TYPE tvep,
       c_real1             TYPE zsdt_depara_cen-centro_real,
       c_real2             TYPE zsdt_depara_cen-centro_real,
       c_werks             TYPE lips-werks,
       vg_remessa_terceiro TYPE c LENGTH 1,
       wa_zlest0026        TYPE zlest0026,
       it_likp             TYPE TABLE OF likp WITH HEADER LINE,
       it_vbpa             TYPE TABLE OF vbpa WITH HEADER LINE,
       wa_konp             TYPE konp,
       oref                TYPE REF TO zcl_memory_ft_entrada_handle.

DATA: vg_pc_veiculo TYPE  zplaca,
      ws_zlest0135  TYPE zlest0135,
      e_status      TYPE sy-subrc.

*-#133089-21.02.2024-JT-inicio
DATA: lc_faturamento_automatico TYPE REF TO zcl_faturamento_automatico,
      vg_faturamento_autom      TYPE char01.
*-#133089-12.02.2024-JT-fim

RANGES: r_vsart FOR tvtk-vsart.


CLEAR: r_vsart, r_vsart[].

r_vsart-sign   = 'I'.
r_vsart-option = 'EQ'.
r_vsart-low    = '01'. "Rodoviario
APPEND r_vsart.

r_vsart-low    = '07'. "Multimodal
APPEND r_vsart.

* Grupo de contas do transportador
READ TABLE c_xvttk INDEX 1 INTO wa_vttk.

*-#133089-12.02.2024-JT-inicio
*----------------------------------
*---verifica fatura automatica
*----------------------------------
SELECT SINGLE *
  FROM zlest0241
  INTO @DATA(_zlest0241)
 WHERE ch_referencia = @wa_vttk-id_romaneio
   AND cancelado     = @abap_false.
IF sy-subrc = 0.
  vg_faturamento_autom = abap_true.
  CREATE OBJECT lc_faturamento_automatico.
ENDIF.

IF ( sy-tcode = 'ZLES0106' ) OR
   ( sy-tcode = 'ZLES0113' ) OR
   ( sy-tcode = 'ZLES0115' ) OR
   ( sy-tcode = 'ZLES0136' ) OR
   ( sy-tcode = 'ZNFE'     ) OR  "*-CS2024000086-26.09.2024-#151423-JT-inicio
   ( sy-tcode = 'ZMM0127'  ) OR
   ( sy-tcode = 'ZLES0200' ).
  vg_faturamento_autom = abap_false.
ENDIF.
*-#133089-12.02.2024-JT-inicio

CALL FUNCTION 'Z_LES_DETERMINA_FROTA'
  EXPORTING
    wk_vttk = wa_vttk
  IMPORTING
    add03   = c_xvttk_wa-add03.

* Controle dos botões de status
CASE i_status.
    "
  WHEN 1.

    EXPORT '' TO MEMORY ID 'ZADTOPED'.

    SELECT * INTO TABLE it_likp
      FROM likp
       FOR ALL ENTRIES IN c_xvttp
     WHERE vbeln EQ c_xvttp-vbeln.

    LOOP AT it_likp.

      CASE it_likp-vbtyp.

        WHEN 'J'.

          vg_remessa_terceiro = space.

          CALL FUNCTION 'Z_LES_TIPO_REMESSA'
            EXPORTING
              p_vbeln  = it_likp-vbeln
            CHANGING
              vg_vstel = vg_remessa_terceiro.

          IF ( NOT vg_remessa_terceiro IS INITIAL ) AND ( wa_vttk-shtyp NE 'Z026' ).
*-#133089-21.02.2024-JT-inicio
            CASE vg_faturamento_autom.
              WHEN abap_off.
                MESSAGE e064(zles) WITH 'Tipo de transporte' wa_vttk-shtyp 'não permitido utilizar com' 'Remessa de Terceiro!' RAISING error.
              WHEN abap_true.
                MESSAGE e064(zles) WITH 'Tipo de transporte' wa_vttk-shtyp 'não permitido utilizar com' 'Remessa de Terceiro!' INTO DATA(l_mesg).
                lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                RAISE error.
            ENDCASE.
*-#133089-21.02.2024-JT-fim
            "          ELSEIF ( VG_REMESSA_TERCEIRO IS INITIAL ) AND ( WA_VTTK-SHTYP EQ 'Z026' ).
            "            MESSAGE E064(ZLES) WITH 'Tipo de transporte' WA_VTTK-SHTYP 'somente permitido utilizar com' 'Remessa de Terceiro!'  RAISING ERROR.

          ELSEIF ( wa_vttk-shtyp EQ 'Z026' ).

            "Ajuste Z026 CS2019000109 - 30.01.2019 - Ini
            DATA: v_emissor_ordem TYPE j_1bparid,
                  v_agente_frete  TYPE j_1bparid.

            CLEAR: v_emissor_ordem, v_agente_frete.

            SELECT SINGLE *
              FROM vbpa INTO @DATA(_wl_vbpa)
             WHERE vbeln EQ @it_likp-vbeln
               AND parvw EQ 'AG'.

            IF sy-subrc EQ 0.
              v_emissor_ordem = _wl_vbpa-kunnr.
              v_agente_frete  = wa_vttk-tdlnr.
            ENDIF.

            IF v_agente_frete IS INITIAL.
*-#133089-21.02.2024-JT-inicio
              CASE vg_faturamento_autom.
                WHEN abap_off.
                  MESSAGE e142(zles) RAISING error.
                WHEN abap_true.
                  MESSAGE e142(zles) INTO l_mesg.
                  lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                  RAISE error.
              ENDCASE.
*-#133089-21.02.2024-JT-fim
            ENDIF.

            IF v_emissor_ordem IS INITIAL.
*-#133089-21.02.2024-JT-inicio
              CASE vg_faturamento_autom.
                WHEN abap_off.
                  MESSAGE e143(zles) WITH it_likp-vbeln RAISING error.
                WHEN abap_true.
                  MESSAGE e143(zles) WITH it_likp-vbeln INTO l_mesg.
                  lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                  RAISE error.
              ENDCASE.
*-#133089-21.02.2024-JT-fimr.
            ENDIF.

            TRY.

                "Agente de Frete deve ser local de negocio
                TRY.
                    zcl_clientes=>zif_parceiros~get_instance(
                     )->set_parceiro( i_parceiro = v_agente_frete
                     )->ck_ativo(
                     )->ck_parceiro_local_negocio( IMPORTING e_j_1bbranch = DATA(wl_branch_ag_frete) ).
                  CATCH zcx_parceiros INTO DATA(ex_parceiros_d).
*-#133089-21.02.2024-JT-inicio
                    CASE vg_faturamento_autom.
                      WHEN abap_off.
                        ex_parceiros_d->published_erro( i_msgty = 'E' i_msgty_display = 'W' ).
                      WHEN abap_true.
                        MESSAGE ID ex_parceiros_d->msgid TYPE 'S' NUMBER ex_parceiros_d->msgno WITH ex_parceiros_d->msgv1 ex_parceiros_d->msgv2 ex_parceiros_d->msgv3 ex_parceiros_d->msgv4 INTO l_mesg.
                        lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                        RAISE error.
                    ENDCASE.
*-#133089-21.02.2024-JT-fimr.
                ENDTRY.

                "Checar se Emissor de Ordem esta ativo.
                TRY.
                    zcl_clientes=>zif_parceiros~get_instance(
                      )->set_parceiro( i_parceiro = v_emissor_ordem
                      )->ck_ativo(
                      )->ck_ativo_empresa( i_empresa =  wl_branch_ag_frete-bukrs ).
                  CATCH zcx_parceiros INTO ex_parceiros_d.
*-#133089-21.02.2024-JT-inicio
                    CASE vg_faturamento_autom.
                      WHEN abap_off.
                        ex_parceiros_d->published_erro( i_msgty = 'E' i_msgty_display = 'W' ).
                      WHEN abap_true.
                        MESSAGE ID ex_parceiros_d->msgid TYPE 'S' NUMBER ex_parceiros_d->msgno WITH ex_parceiros_d->msgv1 ex_parceiros_d->msgv2 ex_parceiros_d->msgv3 ex_parceiros_d->msgv4 INTO l_mesg.
                        lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                        RAISE error.
                    ENDCASE.
*-#133089-21.02.2024-JT-fimr.
                ENDTRY.

                "Checar Emissor de Ordem é um Local de Negocio para prosseguir com validaçoes.
                zcl_clientes=>zif_parceiros~get_instance(
                 )->set_parceiro( i_parceiro = v_emissor_ordem
                 )->ck_parceiro_local_negocio( IMPORTING e_j_1bbranch = DATA(wl_branch_emi_ordem) ).

                "Agente de Frete deve ser de empresa diferente do emissor da Ordem
                TRY.
                    zcl_clientes=>zif_parceiros~get_instance(
                      )->set_parceiro( i_parceiro = v_agente_frete
                      )->ck_parceiro_emp_diferente( EXPORTING  i_empresa  = wl_branch_emi_ordem-bukrs ).
                  CATCH zcx_parceiros INTO ex_parceiros_d.
*-#133089-21.02.2024-JT-inicio
                    CASE vg_faturamento_autom.
                      WHEN abap_off.
                        ex_parceiros_d->published_erro( i_msgty = 'E' i_msgty_display = 'W' ).
                      WHEN abap_true.
                        MESSAGE ID ex_parceiros_d->msgid TYPE 'S' NUMBER ex_parceiros_d->msgno WITH ex_parceiros_d->msgv1 ex_parceiros_d->msgv2 ex_parceiros_d->msgv3 ex_parceiros_d->msgv4 INTO l_mesg.
                        lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                        RAISE error.
                    ENDCASE.
*-#133089-21.02.2024-JT-fimr.
                ENDTRY.

              CATCH zcx_parceiros INTO ex_parceiros_d.
            ENDTRY.
            " Ajuste Z026 CS2019000109 - 30.01.2019 - Fim

          ENDIF.

          IF it_likp-inco1 = 'CIF' AND wa_vttk-vsart IN r_vsart AND wa_vttk-shtyp NE 'Z020' AND wa_vttk-shtyp NE 'Z026'.
            IF it_likp-vstel NE wa_vttk-tplst.
*-#133089-21.02.2024-JT-inicio
              CASE vg_faturamento_autom.
                WHEN abap_off.
                  MESSAGE e064(zles) WITH 'Local de organização de transporte' ' deve ser igual ao local de expedição'  RAISING error.
                WHEN abap_true.
                  MESSAGE e064(zles) WITH 'Local de organização de transporte' ' deve ser igual ao local de expedição'  INTO l_mesg.
                  lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                  RAISE error.
              ENDCASE.
*-#133089-21.02.2024-JT-fimr.
            ENDIF.
          ENDIF.

        WHEN '7'.

          IF wa_vttk-shtyp NE 'Z021' AND wa_vttk-shtyp NE 'Z027' AND wa_vttk-shtyp NE 'Z031'.
*-#133089-21.02.2024-JT-inicio
            CASE vg_faturamento_autom.
              WHEN abap_off.
                MESSAGE e064(zles) WITH 'Tipo de transporte' wa_vttk-shtyp 'não permitido utilizar com' 'Aviso de remessa/entrega!' RAISING error.
              WHEN abap_true.
                MESSAGE e064(zles) WITH 'Tipo de transporte' wa_vttk-shtyp 'não permitido utilizar com' 'Aviso de remessa/entrega!' INTO l_mesg.
                lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                RAISE error.
            ENDCASE.
*-#133089-21.02.2024-JT-fimr.
          ELSEIF wa_vttk-shtyp NE 'Z027' AND wa_vttk-shtyp NE 'Z021' AND wa_vttk-shtyp NE 'Z031'.
*-#133089-21.02.2024-JT-inicio
            CASE vg_faturamento_autom.
              WHEN abap_off.
                MESSAGE e064(zles) WITH 'Tipo de transporte' wa_vttk-shtyp 'não permitido utilizar com' 'Aviso de remessa/entrega!' RAISING error.
              WHEN abap_true.
                MESSAGE e064(zles) WITH 'Tipo de transporte' wa_vttk-shtyp 'não permitido utilizar com' 'Aviso de remessa/entrega!' INTO l_mesg.
                lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                RAISE error.
            ENDCASE.
*-#133089-21.02.2024-JT-fimr.
          ENDIF.
          "
          "CHECAR SE CENTRO EMISSOR E O CENTRO DO DOCUMENTO DE TRANSPORTE SÃO OS MESMOS
          SELECT SINGLE werks
            FROM lips
            INTO c_werks
            WHERE vbeln = it_likp-vbeln.

          SELECT SINGLE centro_real
            FROM zsdt_depara_cen
            INTO c_real1
            WHERE centrov_1 = c_werks.

          IF sy-subrc NE 0.
            c_real1 = c_werks.
          ENDIF.

          SELECT SINGLE centro_real
            FROM zsdt_depara_cen
            INTO c_real2
            WHERE centrov_1 = wa_vttk-tplst.

          IF sy-subrc NE 0.
            c_real2 = wa_vttk-tplst.
          ENDIF.

          IF c_real1 NE c_real2.
*-#133089-21.02.2024-JT-inicio
            CASE vg_faturamento_autom.
              WHEN abap_off.
                MESSAGE e064(zles) WITH 'local de organização de transporte' c_real2 'diferente de centro' c_real1 RAISING error.
              WHEN abap_true.
                MESSAGE e064(zles) WITH 'local de organização de transporte' c_real2 'diferente de centro' c_real1 INTO l_mesg.
                lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                RAISE error.
            ENDCASE.
*-#133089-21.02.2024-JT-fimr.
          ENDIF.


        WHEN OTHERS.

          IF wa_vttk-shtyp EQ 'Z021'.
*-#133089-21.02.2024-JT-inicio
            CASE vg_faturamento_autom.
              WHEN abap_off.
                MESSAGE e064(zles) WITH 'Tipo de transporte' wa_vttk-shtyp 'somente permitido utilizar com' 'Aviso de remessa/entrega!'  RAISING error.
              WHEN abap_true.
                MESSAGE e064(zles) WITH 'Tipo de transporte' wa_vttk-shtyp 'somente permitido utilizar com' 'Aviso de remessa/entrega!'  INTO l_mesg.
                lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                RAISE error.
            ENDCASE.
*-#133089-21.02.2024-JT-fimr.
          ENDIF.

      ENDCASE.
    ENDLOOP.

* Fim de carregamento
  WHEN 4.

* Parceiro Ponto de coleta
    READ TABLE c_xvbpa WITH KEY parvw = 'PC'.

    IF NOT sy-subrc IS INITIAL.
*-#133089-21.02.2024-JT-inicio
      CASE vg_faturamento_autom.
        WHEN abap_off.
          MESSAGE i033(zles) WITH TEXT-002.
        WHEN abap_true.
          MESSAGE i033(zles) WITH TEXT-002 INTO l_mesg..
          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
      ENDCASE.
*-#133089-21.02.2024-JT-fimr.
    ENDIF.

* Parceiro Local de entrega
    READ TABLE c_xvbpa WITH KEY parvw = 'LR'.

    IF NOT sy-subrc IS INITIAL.
*-#133089-21.02.2024-JT-inicio
      CASE vg_faturamento_autom.
        WHEN abap_off.
          MESSAGE i033(zles) WITH TEXT-003.
        WHEN abap_true.
          MESSAGE i033(zles) WITH TEXT-003 INTO l_mesg.
          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
      ENDCASE.
*-#133089-21.02.2024-JT-fimr.
    ENDIF.

* Parceiro Motorista deve ser do Grupo ZMOT
    READ TABLE c_xvbpa WITH KEY parvw = 'MT' INTO DATA(wa_motorista).
    IF sy-subrc IS INITIAL.
      TRY .
          IF CAST zcl_fornecedores( zcl_fornecedores=>zif_parceiros~get_instance( )->set_parceiro( i_parceiro = wa_motorista-lifnr
                ) )->at_lfa1-ktokk NE 'ZMOT'.
*-#133089-21.02.2024-JT-inicio
            CASE vg_faturamento_autom.
              WHEN abap_off.
                MESSAGE e146(zles) WITH wa_motorista-lifnr RAISING error.
              WHEN abap_true.
                MESSAGE e146(zles) WITH wa_motorista-lifnr INTO l_mesg.
                lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                RAISE error.
            ENDCASE.
*-#133089-21.02.2024-JT-fimr.
          ENDIF.
        CATCH zcx_parceiros INTO DATA(ex_parceiros).
*-#133089-21.02.2024-JT-inicio
          CASE vg_faturamento_autom.
            WHEN abap_off.
              MESSAGE ID ex_parceiros->msgid TYPE ex_parceiros->msgty NUMBER ex_parceiros->msgno
                 WITH ex_parceiros->msgv1 ex_parceiros->msgv2 ex_parceiros->msgv3 ex_parceiros->msgv4 RAISING error.
            WHEN abap_true.
              MESSAGE ID ex_parceiros->msgid TYPE ex_parceiros->msgty NUMBER ex_parceiros->msgno
                 WITH ex_parceiros->msgv1 ex_parceiros->msgv2 ex_parceiros->msgv3 ex_parceiros->msgv4 INTO l_mesg.
              lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
              RAISE error.
          ENDCASE.
*-#133089-21.02.2024-JT-fimr.
      ENDTRY.
    ENDIF.

* Validação frete rodoviário
    CHECK  c_xvttk_wa-vsart IN r_vsart.

* Não controla a desflegagem.
    IF c_xvttk_wa-stlad = 'X'.

      CLEAR : c_xvttk_wa-add01, c_xvttk_wa-tndr_trkid, c_xvttk_wa-add02, c_xvttk_wa-text1, c_xvttk_wa-text2,
      c_xvttk_wa-add03, c_xvttk_wa-signi,c_xvttk_wa-text3, c_xvttk_wa-text4.

    ELSE.

      DATA : wk_vbpa      TYPE vbpavb,
             vl_agregado  TYPE zlest0002-agregado,
             vl_placa     TYPE zlest0002-pc_veiculo,
             vl_name1     TYPE lfa1-name1,
             wk_0001      TYPE zsdt0001,
             wk_0002      TYPE zlest0002,
             wa_placa     TYPE zlese0032,
             wk_zlest0108 TYPE zlest0108.

* Controle de impressão de carta frete
      DATA : ti_placa TYPE TABLE OF zlese0032,
             sl_placa TYPE zlese0032.

      CLEAR: wk_vbpa,  vl_agregado, vl_placa, sem_pv.

* Validação de próprio e terceiro
      IF NOT c_xvttk_wa-add03 = '0000000002'.

* Proprietário do veículo

        READ TABLE c_xvbpa INTO wk_vbpa WITH KEY parvw = 'PV'
                                                 posnr = '000000'.

        IF sy-subrc IS INITIAL.

          CALL FUNCTION 'Z_LES_DETERMINA_GRUPO_CONTA'
            EXPORTING
              p_lifnr = wk_vbpa-lifnr
              p_tknum = wa_vttk-tknum
            IMPORTING
              o_add02 = c_xvttk_wa-add02
            EXCEPTIONS
              error   = 1
              OTHERS  = 2.

          IF NOT sy-subrc IS INITIAL.
*-#133089-21.02.2024-JT-inicio
            CASE vg_faturamento_autom.
              WHEN abap_off.
                MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING error.
              WHEN abap_true.
                MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO l_mesg.
                lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                RAISE error.
            ENDCASE.
*-#133089-21.02.2024-JT-fimr.
          ENDIF.

          "ALRS 19.04.2016
* Opus
* Determinação dos parceiros da remessa atribuída ao documento de transporte
          CLEAR: vl_placa , vl_agregado, wk_0001, wk_zlest0108.
          READ TABLE c_xvttp INDEX 1.
          IF sy-subrc IS INITIAL.

            SELECT *
              INTO wk_zlest0108
              FROM zlest0108 UP TO 1 ROWS
               FOR ALL ENTRIES IN c_xvttp
             WHERE vbeln = c_xvttp-vbeln.
            ENDSELECT.

            IF wk_zlest0108 IS INITIAL.

              TRY.
                  DATA(handle) = zcl_memory_ft_entrada=>attach_for_read( inst_name = CONV #( c_xvttp-vbeln ) ).
                  oref ?= handle->root.
                  wk_zlest0108 = oref->at_zlest0108.
                  handle->detach( ).
                CATCH cx_shm_attach_error.
                  SELECT SINGLE *
                    INTO wk_zlest0108
                    FROM zlest0108
                   WHERE doc_transp EQ wa_vttk-tknum.
              ENDTRY.
            ENDIF.

*-CS2021001045 - 03.02.2022 - JT - inicio
            IF wk_zlest0108 IS INITIAL.
              SELECT *
                INTO CORRESPONDING FIELDS OF wk_zlest0108
                FROM zlest0211 UP TO 1 ROWS
                 FOR ALL ENTRIES IN c_xvttp
               WHERE vbeln = c_xvttp-vbeln.
              ENDSELECT.
            ENDIF.
*-CS2021001045 - 03.02.2022 - JT - fim

            IF wk_zlest0108 IS NOT INITIAL.

              SELECT SINGLE pc_veiculo agregado
                INTO (vl_placa , vl_agregado)
                FROM zlest0002
               WHERE pc_veiculo  = wk_zlest0108-placa_cav
                 AND tp_veiculo EQ '0'.

              IF sy-subrc NE 0.
                SELECT SINGLE pc_veiculo agregado
                    INTO (vl_placa , vl_agregado)
                    FROM  zlest0002
                    WHERE proprietario = wk_vbpa-lifnr.
              ENDIF.

            ELSE.
              IF sy-tcode = 'ZLES0136' OR sy-tcode = 'ZNFE' OR "*-CS2024000086-26.09.2024-#151423-JT-inicio
                vg_faturamento_autom = abap_true.  "*-#133089-21.02.2024-JT
                SELECT *
                INTO wk_0001
                FROM zsdt0001 UP TO 1 ROWS
                FOR ALL ENTRIES IN   c_xvttp
                WHERE doc_rem      = c_xvttp-vbeln AND
                      tp_movimento = 'S'.
                ENDSELECT.

                IF ( sy-subrc NE 0 ) OR ( wk_0001 IS INITIAL ).
                  SELECT *
                    INTO wk_0001
                    FROM zsdt0001 UP TO 1 ROWS
                     FOR ALL ENTRIES IN   c_xvttp
                   WHERE doc_aviso    = c_xvttp-vbeln AND
                         tp_movimento = 'S'.
                  ENDSELECT.
                ENDIF.

              ELSEIF sy-tcode = 'ZLES0115'.
                SELECT *
                    INTO wk_0001
                    FROM zsdt0001 UP TO 1 ROWS
                    FOR ALL ENTRIES IN   c_xvttp
                    WHERE doc_aviso     = c_xvttp-vbeln AND
                          tp_movimento = 'S'.
                ENDSELECT.
              ELSE.
                SELECT *
                INTO wk_0001
                FROM zsdt0001 UP TO 1 ROWS
                FOR ALL ENTRIES IN   c_xvttp
                WHERE doc_rem      = c_xvttp-vbeln AND
                      tp_movimento = 'S'.
                ENDSELECT.
              ENDIF.

              SELECT SINGLE pc_veiculo agregado
                INTO (vl_placa , vl_agregado)
                FROM  zlest0002
                WHERE pc_veiculo  = wk_0001-placa_cav
                     AND tp_veiculo EQ '0'.
              IF sy-subrc NE 0.
                SELECT SINGLE pc_veiculo agregado
                    INTO (vl_placa , vl_agregado)
                    FROM  zlest0002
                    WHERE proprietario = wk_vbpa-lifnr.
              ENDIF.

            ENDIF.

          ELSE.
            SELECT SINGLE pc_veiculo agregado
              INTO (vl_placa , vl_agregado)
              FROM  zlest0002
              WHERE proprietario = wk_vbpa-lifnr.
          ENDIF.

          IF sy-subrc IS INITIAL.
* Controle de agregado - Parceiro proprietário cadastrado o veículo Somente por proprietário
            IF vl_agregado = '1'.
              c_xvttk_wa-add01 = '0000000001'.
            ELSE.
              c_xvttk_wa-add01 = '0000000002'.
            ENDIF.

          ELSE.
            c_xvttk_wa-add01 = '0000000002'.
            MESSAGE s008(zles).
          ENDIF.

        ELSE.
          MESSAGE w074(zles) WITH TEXT-004.
          sem_pv = 'X'.
        ENDIF.

* Opus
* Determinação dos parceiros da remessa atribuída ao documento de transporte
        CLEAR: wk_0001, wk_zlest0108.

        READ TABLE c_xvttp INDEX 1.

        IF sy-subrc IS INITIAL.

          IF sy-tcode = 'ZLES0136' OR sy-tcode = 'ZNFE' OR "*-CS2024000086-26.09.2024-#151423-JT-inicio
             vg_faturamento_autom = abap_true.  "*-#133089-21.02.2024-JT
            SELECT *
              INTO wk_0001
              FROM zsdt0001 UP TO 1 ROWS
              FOR ALL ENTRIES IN   c_xvttp
              WHERE doc_rem      = c_xvttp-vbeln AND
                    tp_movimento = 'S'.
            ENDSELECT.

            IF ( sy-subrc NE 0 ) OR ( wk_0001 IS INITIAL ).
              SELECT *
                INTO wk_0001
                FROM zsdt0001 UP TO 1 ROWS
                FOR ALL ENTRIES IN   c_xvttp
                WHERE doc_aviso     = c_xvttp-vbeln AND
                tp_movimento = 'S'.
              ENDSELECT.
            ENDIF.

          ELSEIF sy-tcode = 'ZLES0115'.
            SELECT *
               INTO wk_0001
               FROM zsdt0001 UP TO 1 ROWS
               FOR ALL ENTRIES IN   c_xvttp
               WHERE doc_aviso     = c_xvttp-vbeln AND
               tp_movimento = 'S'.
            ENDSELECT.
          ELSE.
            SELECT *
            INTO wk_0001
            FROM zsdt0001 UP TO 1 ROWS
            FOR ALL ENTRIES IN   c_xvttp
            WHERE doc_rem      = c_xvttp-vbeln AND
                  tp_movimento = 'S'.
            ENDSELECT.
          ENDIF.

          IF NOT sy-subrc IS INITIAL.

* Determinação dos parceiros da remessa atribuída ao documento de transporte
            IF sy-tcode = 'ZLES0136' OR sy-tcode = 'ZNFE' OR "*-CS2024000086-26.09.2024-#151423-JT-inicio
               vg_faturamento_autom = abap_true.  "*-#133089-21.02.2024-JT
              SELECT *
              INTO wk_0001
              FROM zsdt0001 UP TO 1 ROWS
              FOR ALL ENTRIES IN c_xvttp
              WHERE doc_rem      = c_xvttp-vbeln
                AND tp_movimento = 'E'.
              ENDSELECT.

              IF ( sy-subrc NE 0 ) OR ( wk_0001 IS INITIAL ).
                SELECT *
                  INTO wk_0001
                  FROM zsdt0001 UP TO 1 ROWS
                  FOR ALL ENTRIES IN c_xvttp
                  WHERE doc_aviso      = c_xvttp-vbeln
                    AND tp_movimento = 'E'.
                ENDSELECT.
              ENDIF.

            ELSEIF sy-tcode = 'ZLES0115'.
              SELECT *
                INTO wk_0001
                FROM zsdt0001 UP TO 1 ROWS
                FOR ALL ENTRIES IN c_xvttp
                WHERE doc_aviso      = c_xvttp-vbeln
                  AND tp_movimento = 'E'.
              ENDSELECT.
            ELSE.
              SELECT *
              INTO wk_0001
              FROM zsdt0001 UP TO 1 ROWS
              FOR ALL ENTRIES IN c_xvttp
              WHERE doc_rem      = c_xvttp-vbeln
                AND tp_movimento = 'E'.
              ENDSELECT.
            ENDIF.

          ENDIF.

          IF NOT sy-subrc IS INITIAL.

            TRY.
                READ TABLE c_xvttp INDEX 1.
                IF c_xvttp-vbeln IS NOT INITIAL.
                  handle = zcl_memory_ft_entrada=>attach_for_read( inst_name = CONV #( c_xvttp-vbeln ) ).
                  oref ?= handle->root.
                  wk_zlest0108 = oref->at_zlest0108.
                  handle->detach( ).
                ENDIF.
              CATCH cx_shm_attach_error.
                SELECT *
                INTO wk_zlest0108
                FROM zlest0108 UP TO 1 ROWS
                FOR ALL ENTRIES IN c_xvttp
                WHERE vbeln      = c_xvttp-vbeln.
                ENDSELECT.

*-CS2021001045 - 03.02.2022 - JT - inicio
                IF wk_zlest0108 IS INITIAL.
                  SELECT *
                  INTO CORRESPONDING FIELDS OF wk_zlest0108
                  FROM zlest0211 UP TO 1 ROWS
                  FOR ALL ENTRIES IN c_xvttp
                  WHERE vbeln      = c_xvttp-vbeln.
                  ENDSELECT.
                ENDIF.
*-CS2021001045 - 03.02.2022 - JT - fim
            ENDTRY.
          ENDIF.


          IF ( NOT wk_0001 IS INITIAL ) OR ( NOT wk_zlest0108 IS INITIAL ).

            IF ( NOT wk_0001-placa_cav IS INITIAL ) OR ( NOT wk_zlest0108-placa_cav IS INITIAL  ).
              SELECT SINGLE *
                INTO wk_0002
                FROM zlest0002
               WHERE ( ( pc_veiculo  = wk_0001-placa_cav      ) OR
                       ( pc_veiculo  = wk_zlest0108-placa_cav ) )
                 AND tp_veiculo EQ '0'.
              IF ( NOT sy-subrc IS INITIAL ).
                IF ( wk_0001-placa_cav IS NOT INITIAL ).
*-#133089-21.02.2024-JT-inicio
                  CASE vg_faturamento_autom.
                    WHEN abap_off.
                      MESSAGE s076(zles) WITH wk_0001-placa_cav RAISING error.
                    WHEN abap_true.
                      MESSAGE s076(zles) WITH wk_0001-placa_cav INTO l_mesg.
                      lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                      RAISE error.
                  ENDCASE.
*-#133089-21.02.2024-JT-fimr.
                ELSE.
*-#133089-21.02.2024-JT-inicio
                  CASE vg_faturamento_autom.
                    WHEN abap_off.
                      MESSAGE s076(zles) WITH wk_zlest0108-placa_cav RAISING error.
                    WHEN abap_true.
                      MESSAGE s076(zles) WITH wk_zlest0108-placa_cav INTO l_mesg.
                      lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                      RAISE error.
                  ENDCASE.
*-#133089-21.02.2024-JT-fimr.
                ENDIF.
              ENDIF.

            ENDIF.

            IF ( NOT wk_0001-placa_car1 IS INITIAL ) OR ( NOT wk_zlest0108-placa_car1 IS INITIAL ).
              SELECT SINGLE *
                INTO wk_0002
                FROM zlest0002
               WHERE ( ( pc_veiculo EQ wk_0001-placa_car1      ) OR
                       ( pc_veiculo EQ wk_zlest0108-placa_car1 ) )
                 AND tp_veiculo EQ '0'.
              IF sy-subrc IS INITIAL.

                IF ( wk_0001-placa_car1 IS NOT INITIAL ).
*-#133089-21.02.2024-JT-inicio
                  CASE vg_faturamento_autom.
                    WHEN abap_off.
                      MESSAGE s077(zles) WITH wk_0001-placa_car1 RAISING error.
                    WHEN abap_true.
                      MESSAGE s077(zles) WITH wk_0001-placa_car1 INTO l_mesg.
                      lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                      RAISE error.
                  ENDCASE.
*-#133089-21.02.2024-JT-fimr.
                ELSE.
*-#133089-21.02.2024-JT-inicio
                  CASE vg_faturamento_autom.
                    WHEN abap_off.
                      MESSAGE s077(zles) WITH wk_zlest0108-placa_car1 RAISING error.
                    WHEN abap_true.
                      MESSAGE s077(zles) WITH wk_zlest0108-placa_car1 INTO l_mesg.
                      lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                      RAISE error.
                  ENDCASE.
*-#133089-21.02.2024-JT-fimr.
                ENDIF.

              ENDIF.

            ENDIF.

            IF ( NOT wk_0001-placa_car2 IS INITIAL ) OR ( NOT wk_zlest0108-placa_car2 IS INITIAL ).
              SELECT SINGLE *
                INTO wk_0002
                FROM zlest0002
               WHERE ( ( pc_veiculo EQ wk_0001-placa_car2     ) OR
                       ( pc_veiculo EQ wk_zlest0108-placa_car2 ) )
                 AND tp_veiculo EQ '0'.
              IF sy-subrc IS INITIAL.

                IF ( wk_0001-placa_car2 IS NOT INITIAL ).
*-#133089-21.02.2024-JT-inicio
                  CASE vg_faturamento_autom.
                    WHEN abap_off.
                      MESSAGE s077(zles) WITH wk_0001-placa_car2 RAISING error.
                    WHEN abap_true.
                      MESSAGE s077(zles) WITH wk_0001-placa_car2 INTO l_mesg.
                      lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                      RAISE error.
                  ENDCASE.
*-#133089-21.02.2024-JT-fimr.
                ELSE.
*-#133089-21.02.2024-JT-inicio
                  CASE vg_faturamento_autom.
                    WHEN abap_off.
                      MESSAGE s077(zles) WITH wk_zlest0108-placa_car2 RAISING error.
                    WHEN abap_true.
                      MESSAGE s077(zles) WITH wk_zlest0108-placa_car2 INTO l_mesg.
                      lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                      RAISE error.
                  ENDCASE.
*-#133089-21.02.2024-JT-fimr.
                ENDIF.

              ENDIF.
            ENDIF.

            IF ( NOT wk_0001-placa_car3 IS INITIAL ) OR ( NOT wk_zlest0108-placa_car3 IS INITIAL ).
              SELECT SINGLE *
                INTO wk_0002
                FROM zlest0002
               WHERE ( ( pc_veiculo EQ wk_0001-placa_car3     ) OR
                       ( pc_veiculo EQ wk_zlest0108-placa_car3 ) )
                 AND tp_veiculo EQ '0'.
              IF sy-subrc IS INITIAL.
                IF ( wk_0001-placa_car3 IS NOT INITIAL ).
*-#133089-21.02.2024-JT-inicio
                  CASE vg_faturamento_autom.
                    WHEN abap_off.
                      MESSAGE s077(zles) WITH wk_0001-placa_car3 RAISING error.
                    WHEN abap_true.
                      MESSAGE s077(zles) WITH wk_0001-placa_car3 INTO l_mesg.
                      lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                      RAISE error.
                  ENDCASE.
*-#133089-21.02.2024-JT-fimr.
                ELSE.
*-#133089-21.02.2024-JT-inicio
                  CASE vg_faturamento_autom.
                    WHEN abap_off.
                      MESSAGE s077(zles) WITH wk_zlest0108-placa_car3 RAISING error.
                    WHEN abap_true.
                      MESSAGE s077(zles) WITH wk_zlest0108-placa_car3 INTO l_mesg.
                      lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                      RAISE error.
                  ENDCASE.
*-#133089-21.02.2024-JT-fimr.
                ENDIF.
              ENDIF.
            ENDIF.


            "Proprietário
            IF ( ( NOT wk_0001-placa_cav IS INITIAL ) OR
                 ( NOT wk_zlest0108-placa_cav IS INITIAL ) ) AND ( sem_pv EQ 'X' ).

              IF ( wk_0001-placa_cav IS NOT INITIAL ).
                CALL FUNCTION 'Z_LES_DETERMINA_GRUPO_CONTA'
                  EXPORTING
                    p_placa = wk_0001-placa_cav
                    p_tknum = wa_vttk-tknum
                  IMPORTING
                    o_add02 = c_xvttk_wa-add02
                    o_lifnr = wk_vbpa-lifnr
                  TABLES
                    c_xvbpa = c_xvbpa
                  EXCEPTIONS
                    error   = 1
                    OTHERS  = 2.
              ENDIF.

              IF ( wk_zlest0108-placa_cav IS NOT INITIAL ).
                CALL FUNCTION 'Z_LES_DETERMINA_GRUPO_CONTA'
                  EXPORTING
                    p_placa = wk_zlest0108-placa_cav
                    p_tknum = wa_vttk-tknum
                  IMPORTING
                    o_add02 = c_xvttk_wa-add02
                    o_lifnr = wk_vbpa-lifnr
                  TABLES
                    c_xvbpa = c_xvbpa
                  EXCEPTIONS
                    error   = 1
                    OTHERS  = 2.
              ENDIF.

              IF NOT sy-subrc IS INITIAL.
*-#133089-21.02.2024-JT-inicio
                CASE vg_faturamento_autom.
                  WHEN abap_off.
                    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING error.
                  WHEN abap_true.
                    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO l_mesg.
                    lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                    RAISE error.
                ENDCASE.
*-#133089-21.02.2024-JT-fimr.
              ENDIF.

            ENDIF.
* Cavalo
            sy-subrc = 1.
            IF wk_0001-placa_cav IS NOT INITIAL.
              SELECT  SINGLE *
              INTO wk_0002
              FROM zlest0002
              WHERE pc_veiculo  = wk_0001-placa_cav
                AND st_bloqueio <> 'X'.
            ENDIF.

            IF wk_zlest0108-placa_cav IS NOT INITIAL.
              SELECT  SINGLE *
              INTO wk_0002
              FROM zlest0002
              WHERE pc_veiculo  = wk_zlest0108-placa_cav
                AND st_bloqueio <> 'X'.
            ENDIF.

            IF sy-subrc IS INITIAL.
              CLEAR c_xvttk_wa-text1.
              IF NOT wk_0002-pc_veiculo IS INITIAL.
                CONCATENATE wk_0002-pc_veiculo
                            wk_0002-cd_cidade
                       INTO c_xvttk_wa-text1 SEPARATED BY '-'.
              ENDIF.
            ENDIF.

* Carroceria 1
            sy-subrc = 1.
            IF wk_0001-placa_car1 IS NOT INITIAL.

              SELECT  SINGLE *
              INTO wk_0002
              FROM zlest0002
              WHERE pc_veiculo  = wk_0001-placa_car1 AND
                    st_bloqueio <> 'X'.
            ENDIF.

            IF wk_zlest0108-placa_car1 IS NOT INITIAL.

              SELECT  SINGLE *
              INTO wk_0002
              FROM zlest0002
              WHERE pc_veiculo  = wk_zlest0108-placa_car1 AND
                    st_bloqueio <> 'X'.
            ENDIF.

            IF sy-subrc IS INITIAL.

              CLEAR c_xvttk_wa-text2.
              IF NOT wk_0002-pc_veiculo IS INITIAL.
                CONCATENATE wk_0002-pc_veiculo
                            wk_0002-cd_cidade
                       INTO c_xvttk_wa-text2 SEPARATED BY '-'.
              ENDIF.
            ENDIF.

* Carroceria 2
            sy-subrc = 1.

            IF wk_0001-placa_car2 IS NOT INITIAL.
              SELECT  SINGLE  *
              INTO wk_0002
              FROM zlest0002
              WHERE pc_veiculo  = wk_0001-placa_car2 AND
                    st_bloqueio <> 'X'.
            ENDIF.

            IF wk_zlest0108-placa_car2 IS NOT INITIAL.
              SELECT  SINGLE  *
              INTO wk_0002
              FROM zlest0002
              WHERE pc_veiculo  = wk_zlest0108-placa_car2 AND
                    st_bloqueio <> 'X'.
            ENDIF.

            IF sy-subrc IS INITIAL.
              CLEAR c_xvttk_wa-text3.
              IF NOT wk_0002-pc_veiculo IS INITIAL.
                CONCATENATE wk_0002-pc_veiculo
                            wk_0002-cd_cidade
                       INTO c_xvttk_wa-text3 SEPARATED BY '-'.
              ENDIF.
            ENDIF.

* Carroceria 3
            sy-subrc = 1.

            IF wk_0001-placa_car3 IS NOT INITIAL.
              SELECT  SINGLE  *
              INTO wk_0002
              FROM zlest0002
              WHERE pc_veiculo  = wk_0001-placa_car3 AND
                    st_bloqueio <> 'X'.
            ENDIF.

            IF wk_zlest0108-placa_car3 IS NOT INITIAL.
              SELECT  SINGLE  *
              INTO wk_0002
              FROM zlest0002
              WHERE pc_veiculo  = wk_zlest0108-placa_car3 AND
                    st_bloqueio <> 'X'.
            ENDIF.

            IF sy-subrc IS INITIAL.
              CLEAR c_xvttk_wa-text4.
              IF NOT wk_0002-pc_veiculo IS INITIAL.
                CONCATENATE wk_0002-pc_veiculo
                            wk_0002-cd_cidade
                       INTO c_xvttk_wa-text4 SEPARATED BY '-'.
              ENDIF.
            ENDIF.

            IF NOT wk_vbpa-lifnr IS INITIAL AND
              ( ( NOT wk_0001-placa_car1 IS INITIAL ) OR
                ( NOT wk_zlest0108-placa_car1 IS INITIAL ) ) .
* Controle de agregado - Parceiro proprietário cadastrado o veículo por placa
              IF ( wk_0001-placa_cav IS NOT INITIAL ) OR ( wk_zlest0108-placa_cav IS NOT INITIAL ).
                SELECT SINGLE  pc_veiculo agregado
                  FROM zlest0002
                  INTO (vl_placa , vl_agregado)
                WHERE proprietario = wk_vbpa-lifnr
                  AND ( ( pc_veiculo  = wk_0001-placa_cav ) OR ( pc_veiculo   = wk_zlest0108-placa_cav ) ).
              ENDIF.

              IF sy-subrc IS INITIAL.
                CLEAR: c_xvttk_wa-add01.

                IF vl_agregado = '1'.
                  c_xvttk_wa-add01 = '0000000001'.
                ELSE.
                  c_xvttk_wa-add01 = '0000000002'.
                ENDIF.

              ELSE.
                c_xvttk_wa-add01 = '0000000002'.
              ENDIF.

            ENDIF.

          ELSE.
            CLEAR: wa_placa.

            CALL FUNCTION 'Z_LES_VEICULOS'
              EXPORTING
                p_lifnr = wk_vbpa-lifnr
              TABLES
                c_xvttk = c_xvttk
                c_xvtts = c_xvtts
                c_xvttp = c_xvttp
                c_placa = ti_placa.

            IF NOT ti_placa[] IS INITIAL.

              READ TABLE ti_placa INTO wa_placa INDEX 1.
              CLEAR: vl_agregado, vl_placa .

              IF NOT sem_pv IS INITIAL.

                CLEAR: wk_vbpa-lifnr.

                CALL FUNCTION 'Z_LES_DETERMINA_GRUPO_CONTA'
                  EXPORTING
                    p_placa = wa_placa-placa
                    p_tknum = wa_vttk-tknum
                  IMPORTING
                    o_add02 = c_xvttk_wa-add02
                    o_lifnr = wk_vbpa-lifnr
                  TABLES
                    c_xvbpa = c_xvbpa
                  EXCEPTIONS
                    error   = 1
                    OTHERS  = 2.

                IF NOT sy-subrc IS INITIAL.
*-#133089-21.02.2024-JT-inicio
                  CASE vg_faturamento_autom.
                    WHEN abap_off.
                      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING error.
                    WHEN abap_true.
                      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO l_mesg.
                      lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                      RAISE error.
                  ENDCASE.
*-#133089-21.02.2024-JT-fimr.
                ENDIF.

              ENDIF.

              " Controle de agregado - Parceiro proprietário cadastrado o veículo por placa
              SELECT SINGLE  pc_veiculo agregado
              INTO (vl_placa , vl_agregado)
              FROM zlest0002
              WHERE proprietario = wk_vbpa-lifnr
                AND pc_veiculo   = wa_placa-placa .

              IF sy-subrc IS INITIAL.
                CLEAR: c_xvttk_wa-add01.

                IF vl_agregado = '1'.
                  c_xvttk_wa-add01 = '0000000001'.
                ELSE.
                  c_xvttk_wa-add01 = '0000000002'.
                ENDIF.

              ELSE.
                c_xvttk_wa-add01 = '0000000002'.
              ENDIF.

              LOOP AT ti_placa INTO sl_placa.
                CASE sy-tabix.
                  WHEN 1.
                    CLEAR c_xvttk_wa-text1.
                    IF NOT sl_placa-placa IS INITIAL.
                      CONCATENATE sl_placa-placa
                                  sl_placa-cidade
                             INTO c_xvttk_wa-text1 SEPARATED BY '-'.
                    ENDIF.
                  WHEN 2.
                    CLEAR c_xvttk_wa-text2.
                    IF NOT sl_placa-placa IS INITIAL.
                      CONCATENATE sl_placa-placa
                                  sl_placa-cidade
                             INTO c_xvttk_wa-text2 SEPARATED BY '-'.
                    ENDIF.
                  WHEN 3.
                    CLEAR c_xvttk_wa-text3.
                    IF NOT sl_placa-placa IS INITIAL.
                      CONCATENATE sl_placa-placa
                                  sl_placa-cidade
                             INTO c_xvttk_wa-text3 SEPARATED BY '-'.
                    ENDIF.
                  WHEN 4.
                    CLEAR c_xvttk_wa-text4.
                    IF NOT sl_placa-placa IS INITIAL.
                      CONCATENATE sl_placa-placa
                                  sl_placa-cidade
                             INTO c_xvttk_wa-text4 SEPARATED BY '-'.
                    ENDIF.
                ENDCASE.
                CLEAR sl_placa.
              ENDLOOP.
            ELSE.
*-#133089-21.02.2024-JT-inicio
              CASE vg_faturamento_autom.
                WHEN abap_off.
                  MESSAGE s069(zles) RAISING error.
                WHEN abap_true.
                  MESSAGE s069(zles) INTO l_mesg.
                  lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                  RAISE error.
              ENDCASE.
*-#133089-21.02.2024-JT-fimr.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.

        CLEAR: wk_0001, wk_zlest0108.

        READ TABLE c_xvttp INDEX 1.

        IF sy-subrc IS INITIAL.
          IF sy-tcode = 'ZLES0136' OR sy-tcode = 'ZNFE' OR "*-CS2024000086-26.09.2024-#151423-JT-inicio
             vg_faturamento_autom = abap_true.  "*-#133089-21.02.2024-JT
            SELECT *
            INTO wk_0001
            FROM zsdt0001 UP TO 1 ROWS
            FOR ALL ENTRIES IN   c_xvttp
            WHERE doc_rem      = c_xvttp-vbeln
              AND tp_movimento = 'S'.
            ENDSELECT.

            IF ( sy-subrc NE 0 ) OR ( wk_0001 IS INITIAL ).
              SELECT *
                INTO wk_0001
                FROM zsdt0001 UP TO 1 ROWS
                FOR ALL ENTRIES IN   c_xvttp
                WHERE doc_aviso      = c_xvttp-vbeln
                  AND tp_movimento = 'S'.
              ENDSELECT.
            ENDIF.

          ELSEIF sy-tcode = 'ZLES0115'.
            SELECT *
              INTO wk_0001
              FROM zsdt0001 UP TO 1 ROWS
              FOR ALL ENTRIES IN   c_xvttp
              WHERE doc_aviso      = c_xvttp-vbeln
                AND tp_movimento = 'S'.
            ENDSELECT.
          ELSE.
            SELECT *
            INTO wk_0001
            FROM zsdt0001 UP TO 1 ROWS
            FOR ALL ENTRIES IN   c_xvttp
            WHERE doc_rem      = c_xvttp-vbeln
              AND tp_movimento = 'S'.
            ENDSELECT.
          ENDIF.

          IF NOT sy-subrc IS INITIAL.

* Determinação dos parceiros da remessa atribuída ao documento de transporte
            SELECT *
            INTO wk_0001
            FROM zsdt0001 UP TO 1 ROWS
            FOR ALL ENTRIES IN c_xvttp
            WHERE doc_rem      = c_xvttp-vbeln
              AND tp_movimento = 'E'.
            ENDSELECT.

          ENDIF.

          IF NOT sy-subrc IS INITIAL.

            TRY.
                READ TABLE c_xvttp INDEX 1.
                IF c_xvttp-vbeln IS NOT INITIAL.
                  handle = zcl_memory_ft_entrada=>attach_for_read( inst_name = CONV #( c_xvttp-vbeln ) ).
                  oref ?= handle->root.
                  wk_zlest0108 = oref->at_zlest0108.
                  handle->detach( ).
                ENDIF.
              CATCH cx_shm_attach_error.
                SELECT *
                  INTO wk_zlest0108
                  FROM zlest0108 UP TO 1 ROWS
                  FOR ALL ENTRIES IN c_xvttp
                  WHERE vbeln      = c_xvttp-vbeln.
                ENDSELECT.

*-CS2021001045 - 03.02.2022 - JT - inicio
                IF wk_zlest0108 IS INITIAL.
                  SELECT *
                    INTO CORRESPONDING FIELDS OF wk_zlest0108
                    FROM zlest0211 UP TO 1 ROWS
                    FOR ALL ENTRIES IN c_xvttp
                    WHERE vbeln      = c_xvttp-vbeln.
                  ENDSELECT.
                ENDIF.
*-CS2021001045 - 03.02.2022 - JT - fim
            ENDTRY.

* Determinação dos parceiros da remessa atribuída ao documento de transporte
          ENDIF.


          IF ( NOT wk_0001 IS INITIAL ) OR ( NOT wk_zlest0108 IS INITIAL ).

            " Cavalo
            IF ( NOT wk_0001-placa_cav IS INITIAL ) OR ( NOT wk_zlest0108-placa_cav IS INITIAL ) .
              SELECT  SINGLE *
              INTO wk_0002
              FROM zlest0002
              WHERE ( ( pc_veiculo  EQ wk_0001-placa_cav ) OR
                      ( pc_veiculo  EQ wk_zlest0108-placa_cav ) )
                AND st_bloqueio NE 'X'.

              IF sy-subrc IS INITIAL.

                CLEAR c_xvttk_wa-text1.
                IF NOT wk_0002-pc_veiculo IS INITIAL.
                  CONCATENATE wk_0002-pc_veiculo
                              wk_0002-cd_cidade
                         INTO c_xvttk_wa-text1 SEPARATED BY '-'.
                ENDIF.
              ELSEIF wk_0001-placa_cav IS NOT INITIAL.
                CONCATENATE wk_0001-placa_cav wk_0001-region INTO c_xvttk_wa-text1 SEPARATED BY '-'.
              ELSEIF wk_zlest0108-placa_cav IS NOT INITIAL.
                CONCATENATE wk_zlest0108-placa_cav wk_zlest0108-region INTO c_xvttk_wa-text1 SEPARATED BY '-'.
              ENDIF.
            ENDIF.

            "Carroceria 1
            IF ( NOT wk_0001-placa_car1  IS INITIAL ) OR ( NOT wk_zlest0108-placa_car1  IS INITIAL ).
              SELECT  SINGLE *
              INTO wk_0002
              FROM zlest0002
              WHERE ( ( pc_veiculo  EQ wk_0001-placa_car1 ) OR ( pc_veiculo  EQ wk_zlest0108-placa_car1 ) )
                AND st_bloqueio NE 'X'.

              IF sy-subrc IS INITIAL.

                CLEAR c_xvttk_wa-text2.
                IF NOT wk_0002-pc_veiculo IS INITIAL.
                  CONCATENATE wk_0002-pc_veiculo
                              wk_0002-cd_cidade
                         INTO c_xvttk_wa-text2 SEPARATED BY '-'.
                ENDIF.
              ENDIF.
            ENDIF.

            "Carroceria 2
            IF ( NOT wk_0001-placa_car2  IS INITIAL ) OR ( NOT wk_zlest0108-placa_car2  IS INITIAL ).
              SELECT  SINGLE  *
              INTO wk_0002
              FROM zlest0002
              WHERE ( ( pc_veiculo  EQ wk_0001-placa_car2 ) OR ( pc_veiculo  EQ wk_zlest0108-placa_car2 ) )
                AND st_bloqueio NE 'X'.

              IF sy-subrc IS INITIAL.

                CLEAR c_xvttk_wa-text3.
                IF NOT wk_0002-pc_veiculo IS INITIAL.
                  CONCATENATE wk_0002-pc_veiculo
                              wk_0002-cd_cidade
                         INTO c_xvttk_wa-text3 SEPARATED BY '-'.
                ENDIF.
              ENDIF.
            ENDIF.

            "Carroceria 3
            IF ( NOT wk_0001-placa_car3  IS INITIAL ) OR ( NOT wk_zlest0108-placa_car3  IS INITIAL ).
              SELECT  SINGLE  *
              INTO wk_0002
              FROM zlest0002
              WHERE ( ( pc_veiculo  EQ wk_0001-placa_car3 ) OR ( pc_veiculo  EQ wk_zlest0108-placa_car3 ) )
                AND st_bloqueio NE 'X'.

              IF sy-subrc IS INITIAL.

                CLEAR c_xvttk_wa-text4.
                IF NOT wk_0002-pc_veiculo IS INITIAL.
                  CONCATENATE wk_0002-pc_veiculo
                              wk_0002-cd_cidade
                         INTO c_xvttk_wa-text4 SEPARATED BY '-'.
                ENDIF.
              ENDIF.
            ENDIF.

            IF NOT wk_vbpa-lifnr IS INITIAL AND
               ( ( NOT wk_0001-placa_cav IS INITIAL ) OR ( NOT wk_zlest0108-placa_cav IS INITIAL ) ) .
              "Controle de agregado - Parceiro proprietário cadastrado o veículo por placa
              SELECT SINGLE pc_veiculo agregado
                FROM zlest0002
                INTO (vl_placa , vl_agregado)
              WHERE proprietario = wk_vbpa-lifnr
                AND ( ( pc_veiculo   = wk_0001-placa_cav ) OR ( pc_veiculo   = wk_zlest0108-placa_cav ) ).

              IF sy-subrc IS INITIAL.
                CLEAR: c_xvttk_wa-add01.

                IF vl_agregado = '1'.
                  c_xvttk_wa-add01 = '0000000001'.
                ELSE.
                  c_xvttk_wa-add01 = '0000000002'.
                ENDIF.

              ELSE.
                c_xvttk_wa-add01 = '0000000002'.
              ENDIF.

            ENDIF.
          ENDIF.

        ENDIF.
      ENDIF.

***** "Ajuste na consulta situação do transportador / USER HISTORY "66690 / ABAP AOENNING - 21/06/2023. / Alteração na validação para consultar o situação do transportador.

      SELECT * INTO TABLE it_likp
      FROM likp
       FOR ALL ENTRIES IN c_xvttp
     WHERE vbeln EQ c_xvttp-vbeln.

      READ TABLE it_likp INDEX 1.

      "CIF / Rodoviário / Próprio
      IF it_likp-inco1 = 'CIF' AND c_xvttk_wa-vsart IN r_vsart AND zcl_parceiro=>get_parceiro_local_negocio( i_partiner = wa_vttk-tdlnr ) EQ abap_true.


***** "Ajuste na consulta situação do transportador / USER HISTORY "66690 / ABAP AOENNING - 21/06/2023.

*        SELECT SINGLE *
*          FROM tvarvc INTO @DATA(lwa_tvarv_zseg)
*         WHERE name = 'ZLES0136_EXC_ZSEG'
*           AND low  = @wa_vttk-tdlnr.

        CLEAR: vg_pc_veiculo.
        vg_pc_veiculo = c_xvttk_wa-text1+0(7).


        CLEAR: ws_zlest0135, e_status.
        CALL FUNCTION 'Z_LES_EXC_ZSEG'
          EXPORTING
            i_placa       = vg_pc_veiculo
            i_ck_consulta = abap_true
          IMPORTING
            e_status      = e_status
            e_zlest0135   = ws_zlest0135.

        "Ajuste na consulta situação do transportador / USER HISTORY "66690 / AOENNING.

        CASE e_status.
          WHEN 1. "Se houve erro na comunicação da API.

*-#133089-21.02.2024-JT-inicio
            CASE vg_faturamento_autom.
              WHEN abap_off.
                MESSAGE e119(zles) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING error.
              WHEN abap_true.
                MESSAGE e119(zles) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO l_mesg.
                lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                RAISE error.
            ENDCASE.
*-#133089-21.02.2024-JT-fimr.

          WHEN 2. "Check condição tp_transportador "ETC Não equiparado.

*           119  Não existe % p/ desc. de Seguro. Mat.: &1 Agente: &2!
*           120  Não existe % p/ desc. de IOF Mat.: &1 Agente: &2!
*           121  Cancelado % p/ desc. de Seguro. Mat.: &1 Agente: &2!
*           122  Cancelado % p/ desc. de IOF Mat.: &1 Agente: &2!

            LOOP AT it_likp.


              SELECT * INTO TABLE @DATA(it_lips)
                   FROM lips
                    FOR ALL ENTRIES IN @it_likp
                  WHERE vbeln EQ @it_likp-vbeln.

              READ TABLE it_lips INDEX 1 INTO DATA(wa_lips).

              "Identificar se existe valor de seguro e IOF cadastrado, antes da criação do documento de transporte
              SELECT SINGLE *
                FROM a917
                INTO @DATA(wl_a917)
               WHERE kappl EQ 'F'
                 AND kschl EQ 'ZSEG'
                 AND matnr EQ @wa_lips-matnr
                 AND tdlnr EQ @wa_vttk-tdlnr
                 AND kfrst EQ ''
                 AND datbi GE @sy-datum.

              IF sy-subrc IS NOT INITIAL OR wl_a917-knumh IS INITIAL.
*-#133089-21.02.2024-JT-inicio
                CASE vg_faturamento_autom.
                  WHEN abap_off.
                    MESSAGE e119(zles) WITH wa_lips-matnr wa_vttk-tdlnr RAISING error.
                  WHEN abap_true.
                    MESSAGE e119(zles) WITH wa_lips-matnr wa_vttk-tdlnr INTO l_mesg.
                    lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                    RAISE error.
                ENDCASE.
*-#133089-21.02.2024-JT-fimr.
              ELSE.
                wa_konp-knumh = wl_a917-knumh.
                wa_konp-konwa = '%'.

                SELECT SINGLE * INTO wa_konp
                  FROM konp
                 WHERE knumh    EQ wa_konp-knumh
                   AND konwa    EQ wa_konp-konwa
                   AND loevm_ko EQ abap_false.

                IF sy-subrc IS NOT INITIAL.
*-#133089-21.02.2024-JT-inicio
                  CASE vg_faturamento_autom.
                    WHEN abap_off.
                      MESSAGE e121(zles) WITH wa_lips-matnr wa_vttk-tdlnr RAISING error.
                    WHEN abap_true.
                      MESSAGE e121(zles) WITH wa_lips-matnr wa_vttk-tdlnr INTO l_mesg.
                      lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                      RAISE error.
                  ENDCASE.
*-#133089-21.02.2024-JT-fimr.
                ENDIF.
              ENDIF.

              "Identificar se existe valor de seguro e IOF cadastrado, antes da criação do documento de transporte
              SELECT SINGLE *
                FROM a917
                INTO wl_a917
               WHERE kappl EQ 'F'
                 AND kschl EQ 'ZIOF'
                 AND matnr EQ wa_lips-matnr
                 AND tdlnr EQ wa_vttk-tdlnr
                 AND kfrst EQ ''
                 AND datbi GE sy-datum.

              IF sy-subrc IS NOT INITIAL OR wl_a917-knumh IS INITIAL.
*-#133089-21.02.2024-JT-inicio
                CASE vg_faturamento_autom.
                  WHEN abap_off.
                    MESSAGE e120(zles) WITH wa_lips-matnr wa_vttk-tdlnr RAISING error.
                  WHEN abap_true.
                    MESSAGE e120(zles) WITH wa_lips-matnr wa_vttk-tdlnr INTO l_mesg.
                    lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                    RAISE error.
                ENDCASE.
*-#133089-21.02.2024-JT-fimr.
              ELSE.
                wa_konp-knumh = wl_a917-knumh.
                wa_konp-konwa = '%'.

                SELECT SINGLE * INTO wa_konp
                  FROM konp
                 WHERE knumh    EQ wa_konp-knumh
                   AND konwa    EQ wa_konp-konwa
                   AND loevm_ko EQ abap_false.

                IF sy-subrc IS NOT INITIAL.
*-#133089-21.02.2024-JT-inicio
                  CASE vg_faturamento_autom.
                    WHEN abap_off.
                      MESSAGE e122(zles) WITH wa_lips-matnr wa_vttk-tdlnr RAISING error.
                    WHEN abap_true.
                      MESSAGE e122(zles) WITH wa_lips-matnr wa_vttk-tdlnr INTO l_mesg.
                      lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                      RAISE error.
                  ENDCASE.
*-#133089-21.02.2024-JT-fimr.
                ENDIF.
              ENDIF.

            ENDLOOP.
          WHEN 3. "Se não localizou dados na API.
*-#133089-21.02.2024-JT-inicio
            CASE vg_faturamento_autom.
              WHEN abap_off.
                MESSAGE e119(zles) WITH ws_zlest0135-ds_msg_transportador RAISING error.
              WHEN abap_true.
                MESSAGE e119(zles) WITH ws_zlest0135-ds_msg_transportador INTO l_mesg.
                lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                RAISE error.
            ENDCASE.
*-#133089-21.02.2024-JT-fimr.
          WHEN OTHERS.
        ENDCASE.
      ENDIF.
    ENDIF.

* Processamento para transporte
  WHEN 5.


*--------------------------------------------------------------------------------------------------------*
*   "Salvar dados referente documento VT US 117449 / AOENNING / 06-07-2023
*--------------------------------------------------------------------------------------------------------*
    IF  ( c_xvttk_wa-tknum IS NOT INITIAL ) AND ( c_xvttk_wa-text1 IS NOT INITIAL ).
      CLEAR: vg_pc_veiculo.
      vg_pc_veiculo = c_xvttk_wa-text1+0(7).


      "Função gravar informações no documento da VT.
      CALL FUNCTION 'Z_LES_SAVE_TEXT_VT'
        EXPORTING
          i_tknum     = c_xvttk_wa-tknum
          i_placa_cav = vg_pc_veiculo
          i_id_text   = 'CM18'.
    ENDIF.

*--------------------------------------------------------------------------------------------------------*
*   "Fim da implementação salvar dados referente documento VT US 117449 / AOENNING / 06-07-2023
*--------------------------------------------------------------------------------------------------------*              .

* Ponto de coleta
    READ TABLE c_xvbpa WITH KEY parvw = 'PC'.

    IF NOT sy-subrc IS INITIAL.
*-#133089-21.02.2024-JT-inicio
      CASE vg_faturamento_autom.
        WHEN abap_off.
          MESSAGE i033(zles) WITH TEXT-002.
        WHEN abap_true.
          MESSAGE i033(zles) WITH TEXT-002 INTO l_mesg.
          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
      ENDCASE.
*-#133089-21.02.2024-JT-fimr.
    ENDIF.

* Local de entrega
    READ TABLE c_xvbpa WITH KEY parvw = 'LR'.

    IF NOT sy-subrc IS INITIAL.
*-#133089-21.02.2024-JT-inicio
      CASE vg_faturamento_autom.
        WHEN abap_off.
          MESSAGE i033(zles) WITH TEXT-003.
        WHEN abap_true.
          MESSAGE i033(zles) WITH TEXT-003 INTO l_mesg.
          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
      ENDCASE.
*-#133089-21.02.2024-JT-fimr.
    ENDIF.

* Validação frete rodoviário
    CHECK  c_xvttk_wa-vsart IN r_vsart.

* Validação de próprio e terceiro
    IF NOT c_xvttk_wa-add03 = '0000000002'.

* Proprietário do veículo
      READ TABLE c_xvbpa WITH KEY parvw = 'PV'.

      IF NOT sy-subrc IS INITIAL.
*-#133089-21.02.2024-JT-inicio
        CASE vg_faturamento_autom.
          WHEN abap_off.
            MESSAGE i033(zles) WITH TEXT-004.
          WHEN abap_true.
            MESSAGE i033(zles) WITH TEXT-004 INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
        ENDCASE.
*-#133089-21.02.2024-JT-fimr.
      ENDIF.

* Motorista do veículo
      READ TABLE c_xvbpa WITH KEY parvw = 'MT'.

      IF NOT sy-subrc IS INITIAL.
*-#133089-21.02.2024-JT-inicio
        CASE vg_faturamento_autom.
          WHEN abap_off.
            MESSAGE i033(zles) WITH TEXT-005.
          WHEN abap_true.
            MESSAGE i033(zles) WITH TEXT-005 INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
        ENDCASE.
*-#133089-21.02.2024-JT-fimr.
      ENDIF.

    ENDIF.

* Validação dos flags de status, operação desativada para não confirmação dos status
    IF c_xvttk_wa-stabf = space.

      CHECK c_xvttk_wa-vsart IN r_vsart.

* Controle de impressão de carta frete
      DATA : vl_ctrl   TYPE c,
             vl_margem TYPE zlest0103-margadto,
             vl_tplst  TYPE vttk-tplst,
             vl_tdlnr	 TYPE tdlnr,
             vl_bukrs  TYPE ttds-bukrs.

      TRY.
          READ TABLE c_xvttp INDEX 1.
          IF c_xvttp-vbeln IS NOT INITIAL.
            handle = zcl_memory_ft_entrada=>attach_for_read( inst_name = CONV #( c_xvttp-vbeln ) ).
            oref ?= handle->root.
            wk_zlest0108 = oref->at_zlest0108.
            handle->detach( ).
          ENDIF.
        CATCH cx_shm_attach_error.
          "Verificar se o Pedágio/Adiantamento é Automático
          SELECT SINGLE *
            INTO wk_zlest0108
            FROM zlest0108
           WHERE doc_transp EQ c_xvttk_wa-tknum.

*-CS2021001045 - 03.02.2022 - JT - inicio
          IF wk_zlest0108 IS INITIAL.
            SELECT SINGLE *
              INTO CORRESPONDING FIELDS OF wk_zlest0108
              FROM zlest0211
             WHERE doc_transp EQ c_xvttk_wa-tknum.
          ENDIF.
*-CS2021001045 - 03.02.2022 - JT - fim
      ENDTRY.


* Valor do adiantamento
* Local de organizaçao de transporte
      IF c_xvttk_wa-add03 NE '0000000002'.

        IF ( sy-tcode = 'ZLES0106' ) OR ( sy-tcode = 'ZLES0113' ) OR ( sy-tcode = 'ZLES0115' ) OR
           ( sy-tcode = 'ZLES0136' ) OR ( sy-tcode = 'ZMM0127' ) OR ( wk_zlest0108 IS NOT INITIAL ) OR
           ( sy-tcode = 'ZNFE' )  "*-CS2024000086-26.09.2024-#151423-JT-inicio
          OR ( vg_faturamento_autom = abap_true ).  "*-#133089-21.02.2024-JT
          c_xvttk-tplst = c_xvttk_wa-tplst.
          c_xvttk-tknum = c_xvttk_wa-tknum.
        ENDIF.

        SELECT SINGLE bukrs
          INTO vl_bukrs
          FROM ttds
         WHERE tplst = c_xvttk-tplst.

        SELECT SINGLE * INTO wa_filial
          FROM j_1bbranch
         WHERE bukrs  EQ vl_bukrs
           AND branch EQ c_xvttk-tplst.

        IF NOT sy-subrc IS INITIAL.
          SELECT SINGLE * INTO wa_real
            FROM zsdt_depara_cen
           WHERE vkorg     EQ vl_bukrs
             AND centrov_1 EQ c_xvttk-tplst.

          IF sy-subrc IS INITIAL.
            SELECT SINGLE * INTO wa_filial
              FROM j_1bbranch
             WHERE bukrs  EQ wa_real-vkorg
               AND branch EQ wa_real-centro_real.
          ENDIF.
        ENDIF.

        IF sy-subrc IS INITIAL.
          IF c_xvttk-text1 IS NOT INITIAL.
            SELECT SINGLE  * INTO wk_0002 FROM zlest0002 WHERE pc_veiculo EQ c_xvttk-text1(7).
            IF sy-subrc IS INITIAL.
              SELECT SINGLE margadto INTO vl_margem
                FROM zlest0103
               WHERE bukrs  EQ vl_bukrs
                 AND branch EQ wa_filial-branch
                 AND tdlnr  EQ wk_0002-proprietario.
            ENDIF.
          ELSE.
            sy-subrc = 4.
          ENDIF.

          IF sy-subrc IS NOT INITIAL.
            SELECT SINGLE margadto INTO vl_margem
              FROM zlest0103
             WHERE bukrs  EQ vl_bukrs
               AND branch EQ wa_filial-branch
               AND tdlnr  EQ space.
          ENDIF.
        ENDIF.

        IF sy-subrc IS NOT INITIAL.
*-#133089-21.02.2024-JT-inicio
          CASE vg_faturamento_autom.
            WHEN abap_off.
              MESSAGE e031(zles) WITH vl_bukrs c_xvttk-tplst RAISING error.
            WHEN abap_true.
              MESSAGE e031(zles) WITH vl_bukrs c_xvttk-tplst INTO l_mesg.
              lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
              RAISE error.
          ENDCASE.
*-#133089-21.02.2024-JT-fimr.
        ENDIF.

* Controle de adiantamento e pedágio
        CALL FUNCTION 'Z_LES_ADTO_PEDAGIO'
          EXPORTING
            start_column = 80
            start_row    = 02
            end_column   = 130
            end_row      = 20
            tknum        = c_xvttk-tknum
            i_zlest0108  = wk_zlest0108
          TABLES
            c_xvttk      = c_xvttk
            c_xvtts      = c_xvtts
          EXCEPTIONS
            erro         = 1
            pedagio      = 2
            OTHERS       = 3.

        IF sy-subrc EQ 2.
*-#133089-21.02.2024-JT-inicio
          CASE vg_faturamento_autom.
            WHEN abap_off.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING error.
            WHEN abap_true.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO l_mesg.
              lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
              RAISE error.
          ENDCASE.
*-#133089-21.02.2024-JT-fimr.
        ENDIF.

* Controle de calculo de custo de frete , status = 5 Transferencia para o programa IF_EX_BADI_LE_SHIPMENT~AT_SAVE
        IF NOT sy-subrc IS INITIAL.
          vl_ctrl = '5'.
          EXPORT vl_ctrl FROM vl_ctrl TO MEMORY ID 'CALCCUSTO'.
        ELSE.
          EXPORT vl_ctrl FROM vl_ctrl TO MEMORY ID 'CALCCUSTO'.
        ENDIF.

      ENDIF.
    ELSE.

      CLEAR: wa_zlest0026.

      CALL METHOD zcl_repom_viagem_vpr=>get_id_proc_cliente_vt
        EXPORTING
          i_tknum           = c_xvttk-tknum
        RECEIVING
          e_id_proc_cliente = wa_zlest0026-id_proc_cliente
        EXCEPTIONS
          nao_encontrado    = 1
          OTHERS            = 2.

      IF wa_zlest0026-id_proc_cliente IS NOT INITIAL.
        MESSAGE w061(zrepom).
        CALL FUNCTION 'Z_REPOM_MONITOR_PEDAGIO'
          EXPORTING
            i_incluir = abap_false
            i_tknum   = c_xvttk-tknum.
      ENDIF.

      CLEAR: wa_zlest0026.
      CALL METHOD zcl_repom_viagem_vpr=>get_id_proc_cliente_vt
        EXPORTING
          i_tknum           = c_xvttk-tknum
        RECEIVING
          e_id_proc_cliente = wa_zlest0026-id_proc_cliente
        EXCEPTIONS
          nao_encontrado    = 1
          OTHERS            = 2.

      IF wa_zlest0026-id_proc_cliente IS NOT INITIAL.
*-#133089-21.02.2024-JT-inicio
        CASE vg_faturamento_autom.
          WHEN abap_off.
            MESSAGE e062(zrepom) WITH wa_zlest0026-id_proc_cliente RAISING erro.
          WHEN abap_true.
            MESSAGE e062(zrepom) WITH wa_zlest0026-id_proc_cliente INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_vttk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
            RAISE error.
        ENDCASE.
*-#133089-21.02.2024-JT-fimr.
      ENDIF.

      DELETE FROM zlest0026 WHERE tknum = c_xvttk-tknum.

    ENDIF.

* Status para o início do transporte
  WHEN 6.

    SELECT * INTO TABLE it_likp
      FROM likp
       FOR ALL ENTRIES IN c_xvttp
     WHERE vbeln EQ c_xvttp-vbeln.

    CASE c_xvttk-sttbg.
      WHEN abap_false.

*-CS2021000508 - 27.05.2021 - JT - inicio
*-verifica se OV ou Pedido compras é troca nota
        READ TABLE it_likp INTO DATA(wa_likp) INDEX 1.

*        SELECT *
*          INTO @DATA(wa_lips_tn)
*          FROM lips
*            UP TO 1 ROWS
*         WHERE vbeln EQ @wa_likp-vbeln.
*        ENDSELECT.
*
**-------Ordem de venda
*        SELECT ztrocanota
*          INTO @DATA(l_ztrocanota_ov)
*          FROM vbak
*            UP TO 1 ROWS
*         WHERE vbeln EQ @wa_lips_tn-vgbel.
*        ENDSELECT.
*
**-------Pedido compras
*        IF sy-subrc <> 0.
*          SELECT ztrocanota
*            INTO @DATA(l_ztrocanota_pc)
*            FROM ekpo
*              UP TO 1 ROWS
*           WHERE ebeln EQ @wa_lips_tn-vgbel
*             AND ebelp EQ @wa_lips_tn-vgpos.
*          ENDSELECT.
*        ENDIF.
*
*------------------------------------------
*-------inicio transporte Troca Nota
*------------------------------------------
*        IF l_ztrocanota_ov = abap_true OR
*           l_ztrocanota_pc = abap_true.

*---------------------------------------
*-- valida se troca nota
*---------------------------------------
        IF zcl_faturamento=>zif_faturamento~get_romaneio_trocanota(
                     EXPORTING i_vbeln = wa_likp-vbeln             ) = abap_true.

          "Início do Transporte
*          TRY .
*              zcl_integracao_trocant_aprovar=>zif_integracao_trocant_aprovar~get_instance(
*                )->set_viagem_carregar(
*                EXPORTING
*                  i_dt_carregamento        = sy-datlo    " Data Carregamento
*                  i_remessas               = it_likp[]
*                  i_tknum                  = wa_vttk-tknum
*                ).
*            CATCH zcx_integracao INTO DATA(ex_integracao_tn).
*
*              IF NOT ( ex_integracao_tn->msgid EQ zcx_integracao=>zcx_servico_http_config-msgid AND
*                       ex_integracao_tn->msgno EQ zcx_integracao=>zcx_servico_http_config-msgno ).
*                MESSAGE ID ex_integracao_tn->msgid TYPE 'E'
*                NUMBER ex_integracao_tn->msgno WITH ex_integracao_tn->msgv1 ex_integracao_tn->msgv2 ex_integracao_tn->msgv3 ex_integracao_tn->msgv4 RAISING erro.
*              ENDIF.
*
*            CATCH zcx_error INTO DATA(ex_error_tn).
*              MESSAGE ID ex_error_tn->msgid TYPE 'E'
*              NUMBER ex_error_tn->msgno WITH ex_error_tn->msgv1 ex_error_tn->msgv2 ex_error_tn->msgv3 ex_error_tn->msgv4 RAISING erro.
*
*          ENDTRY.

        ELSE.

*------------------------------------------
*-------inicio transporte normal
*------------------------------------------
          DATA: l_tentar TYPE i.     "*-#167847-24.02.2025-JT-inicio

          "Início do Transporte
          DO 6 TIMES.                "*-#167847-24.02.2025-JT-inicio
            l_tentar = sy-index. "*-#167847-24.02.2025-JT-inicio

            TRY .
                zcl_integracao_viagem_carregar=>zif_integracao_viagem_carregar~get_instance(
                  )->set_viagem_carregar(
                  EXPORTING
                    i_dt_carregamento        = sy-datlo    " Data Carregamento
                    i_remessas               = it_likp[]
                    i_tknum                  = wa_vttk-tknum
                  ).

*-#167847-24.02.2025-JT-inicio
                CASE vg_faturamento_autom.
                  WHEN abap_off.
                    EXIT.
                  WHEN abap_true.
                    l_mesg = 'Integração com Carguero ocorreu com Sucesso.'.
                    lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia  = wa_vttk-id_romaneio i_type = 'S' i_msg = l_mesg i_status = 'TRAN' ). "*-#167847-24.02.2025-JT-inicio
                    EXIT.
                ENDCASE.
*-#167847-24.02.2025-JT-fim

              CATCH zcx_integracao INTO DATA(ex_integracao).

                IF NOT ( ex_integracao->msgid EQ zcx_integracao=>zcx_servico_http_config-msgid AND
                         ex_integracao->msgno EQ zcx_integracao=>zcx_servico_http_config-msgno ).
*-#133089-21.02.2024-JT-inicio
                  CASE vg_faturamento_autom.
                    WHEN abap_off.
                      MESSAGE ID ex_integracao->msgid TYPE 'E'
                      NUMBER ex_integracao->msgno WITH ex_integracao->msgv1 ex_integracao->msgv2 ex_integracao->msgv3 ex_integracao->msgv4 RAISING erro.
                    WHEN abap_true.
                      MESSAGE ID ex_integracao->msgid TYPE 'E'
                      NUMBER ex_integracao->msgno WITH ex_integracao->msgv1 ex_integracao->msgv2 ex_integracao->msgv3 ex_integracao->msgv4 INTO l_mesg.
                      lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia  = wa_vttk-id_romaneio i_type   = 'E' i_msg = l_mesg
                                                                      i_erro_abandonar = '01'                i_status = 'TRAN' ). "*-#167847-24.02.2025-JT-inicio

*-#167847-24.02.2025-JT-inicio
                      IF l_tentar <= 5.
                        WAIT UP TO 5 SECONDS.
                        CONTINUE.
                      ELSE.
                        RAISE error.
                      ENDIF.
*-#167847-24.02.2025-JT-fim
                  ENDCASE.
*-#133089-21.02.2024-JT-fimr.
                ENDIF.

              CATCH zcx_error INTO DATA(ex_error).
*-#133089-21.02.2024-JT-inicio
                CASE vg_faturamento_autom.
                  WHEN abap_off.
                    MESSAGE ID ex_error->msgid TYPE 'E'
                    NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2 ex_error->msgv3 ex_error->msgv4 RAISING erro.
                  WHEN abap_true.
                    MESSAGE ID ex_error->msgid TYPE 'E'
                    NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2 ex_error->msgv3 ex_error->msgv4 INTO l_mesg.
                    lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia  = wa_vttk-id_romaneio i_type   = 'E' i_msg = l_mesg
                                                                    i_erro_abandonar = '01'                i_status = 'TRAN' ). "*-#167847-24.02.2025-JT-inicio
*-#167847-24.02.2025-JT-inicio
                    IF l_tentar <= 5.
                      WAIT UP TO 5 SECONDS.
                      CONTINUE.
                    ELSE.
                      RAISE error.
                    ENDIF.
*-#167847-24.02.2025-JT-fim
                ENDCASE.
*-#133089-21.02.2024-JT-fimr.
            ENDTRY.
          ENDDO.  "*-#167847-24.02.2025-JT-fim

          PERFORM f_inf_car_frete_sem_pesagem TABLES it_likp
                                               USING wa_vttk-tknum.


        ENDIF.
*-CS2021000508 - 27.05.2021 - JT - fim

      WHEN abap_true.
        "Cancelando Início do Transporte

    ENDCASE.

*    CHECK NOT c_xvttk-sttbg IS INITIAL.
*
*    DATA: vl_sales  TYPE vbak-vbeln        ,
*          tl_return TYPE TABLE OF bapiret2,
*          vl_fknum  TYPE vfkp-fknum.

*    IF NOT c_xvttk-exti2 IS INITIAL.

* Validação de próprio e terceiro
*      IF c_xvttk_wa-vsart = '01' AND c_xvttk_wa-add03 = '0000000002'.
*        EXIT.
*      endif.
*
*      IF c_xvttk-tknum(01) EQ '$' OR
*         c_xvttk-tknum     IS INITIAL.
*        MESSAGE i043(zles).
*      ELSE.
*
** Verifica se ja foi criado ordem de vendas de serviço para o documento
*        SELECT SINGLE vbeln
*        FROM vbak
*        INTO vl_vbeln
*        WHERE  tknum EQ c_xvttk-tknum.
*
*        IF NOT sy-subrc IS INITIAL.
*
** Verifica se ha documento de custo de frete para o documento.
*          SELECT SINGLE fknum
*          INTO  vl_fknum
*          FROM  vfkp
*          WHERE rebel  = c_xvttk-tknum AND
*                refty  = '8'.
*
** Criar Ordem e fatura de serviço.
*          IF sy-subrc IS INITIAL.
*
*            CALL FUNCTION 'ZSD_OV_ZTRO'
*              EXPORTING
*                p_tknum  = c_xvttk-tknum
*              IMPORTING
*                p_sales  = vl_sales
*                t_return = tl_return.
*
*          ENDIF.
*
*        ELSE.
*
*          MESSAGE i042(zles).
*
*        ENDIF.
*
*      ENDIF.

*    ELSE.
*      MESSAGE i045(zles).
*    ENDIF.

* Fim de transporte
  WHEN 7.

    CHECK  c_xvttk_wa-vsart IN r_vsart.

* Validação dos flags de status, operação desativada para não confirmação dos status
    IF c_xvttk_wa-stten = space.

* Controle de impressão de carta frete
      DATA : wk_0014 LIKE zlest0014,
             vl_lote LIKE zlest0013-lote.

      " Comentado CSB 11/07/2011
*      CLEAR: vl_name1,  wk_0014, vl_lote.
*
*      SELECT SINGLE mandt tknum conhec ctafrete
*      INTO wk_0014
*      FROM zlest0014
*      WHERE tknum = c_xvttk-tknum.
*
*      IF NOT sy-subrc IS INITIAL.
*        MESSAGE s009(zles).
*      ELSE.
*        c_xvttk_wa-exti1 = wk_0014-conhec.
*        c_xvttk_wa-exti2 = wk_0014-ctafrete.
*      ENDIF.
      " Ate aqui
* Determinação do número do lote
      SELECT SINGLE lote
      INTO  vl_lote
      FROM  zlest0013
      WHERE codtrp   = c_xvttk-tdlnr    AND
            conhec   = c_xvttk_wa-exti1 AND
            ctafrete = c_xvttk_wa-exti2.

      IF sy-subrc IS INITIAL.
        c_xvttk_wa-signi = vl_lote.
      ENDIF.

    ELSE.
      CLEAR : c_xvttk_wa-exti1, c_xvttk_wa-exti2, c_xvttk_wa-signi.
    ENDIF.

ENDCASE.
