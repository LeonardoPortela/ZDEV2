*&---------------------------------------------------------------------*
*&  Include           ZXV56U11
*&---------------------------------------------------------------------*

* Permitido apenas para atualização nas transações do documento de transporte

DATA: wk_aux                    TYPE vttpvb,
      wk_auxk                   TYPE vttkvb,
      wk_zlest0108              TYPE zlest0108,
*-#133089-21.02.2024-JT-inicio
      lc_faturamento_automatico TYPE REF TO zcl_faturamento_automatico,
      vg_faturamento_autom      TYPE char01.
*-#133089-12.02.2024-JT-fim

DATA: oref      TYPE REF TO zcl_memory_ft_entrada_handle.


RANGES: r_vsart FOR tvtk-vsart.


CLEAR: r_vsart, r_vsart[].

r_vsart-sign   = 'I'.
r_vsart-option = 'EQ'.
r_vsart-low    = '01'. "Rodoviario
APPEND r_vsart.

r_vsart-low    = '07'. "Multimodal
APPEND r_vsart.

* Informações dos items do documento de transporte.
READ TABLE i_xvttk INTO wk_auxk INDEX 1.
READ TABLE i_xvttp INTO wk_aux  INDEX 1.

IF sy-subrc IS INITIAL AND wk_auxk-id_carga IS INITIAL.

  TRY.
      DATA(handle) = zcl_memory_ft_entrada=>attach_for_read( inst_name = CONV #( wk_aux-vbeln ) ).
      oref ?= handle->root.
      wk_zlest0108 = oref->at_zlest0108.
      IF wk_zlest0108 IS NOT INITIAL.
        DATA(lc_deixa_seguir) = abap_true.
      ENDIF.
      handle->detach( ).
    CATCH cx_shm_attach_error.
  ENDTRY.
ELSEIF wk_auxk-id_carga IS NOT INITIAL .
  lc_deixa_seguir = abap_true.
ENDIF.

*DATA(LC_TEXTO1) = 'CPROG=' && SY-CPROG && ',XPROG=' && SY-XPROG.
*DATA(LC_TEXTO2) = ',XFORM=' && SY-XFORM && ',TCODE=' && SY-TCODE.
*DATA(LC_TEXTO3) = ',REPI2=' && SY-REPI2.
*MESSAGE S000(ZAQUAVIARIO) WITH LC_TEXTO1 LC_TEXTO2 LC_TEXTO3.

*-#133089-12.02.2024-JT-inicio
*----------------------------------
*---verifica fatura automatica
*----------------------------------
SELECT SINGLE *
  FROM zlest0241
  INTO @DATA(_zlest0241)
 WHERE ch_referencia = @wk_auxk-id_romaneio
   AND cancelado     = @abap_false.
IF sy-subrc = 0.
  vg_faturamento_autom = abap_true.
  CREATE OBJECT lc_faturamento_automatico.

  IF sy-tcode(2)  = 'VT'       OR
     sy-tcode     = 'ZOPUS'    OR
     sy-tcode     = 'ZTRANSF'  OR
     sy-tcode     = 'ZLES0060' OR
   ( sy-tcode     = 'ZLES0086'    OR
     sy-cprog     = 'ZLESR0077' ) OR
     sy-tcode     = 'ZLES0106' OR
     sy-tcode     = 'ZLES0113' OR
     sy-tcode     = 'ZLES0115' OR
     sy-tcode     = 'ZLES0136' OR
     sy-tcode     = 'ZNFE'     OR "*-CS2024000086-26.09.2024-#151423-JT-inicio
     sy-tcode     = 'ZMM0079'  OR
     sy-tcode     = 'ZMM0127'  OR
     sy-tcode     = 'ZLES0200' OR
     sy-cprog     = 'ZMMR183'  OR
     sy-cprog     = 'ZMMR105'  OR "178025 CS2023000574 Job dinâmico PSA
     sy-cprog     = 'ZMMR0208' OR "178025 CS2023000574 Job dinâmico PSA
     sy-batch     = 'X'.          "178025 CS2023000574 Job dinâmico PSA
    vg_faturamento_autom = abap_false.
  ENDIF.
ENDIF.
*-#133089-12.02.2024-JT-inicio

CHECK sy-tcode(2) NE 'VI'.

CHECK sy-tcode(2)     = 'VT'       OR
      sy-tcode        = 'ZOPUS'    OR
      sy-tcode        = 'ZTRANSF'  OR
      sy-tcode        = 'ZLES0060' OR
      ( sy-tcode = 'ZLES0086' OR sy-cprog = 'ZLESR0077' ) OR
      sy-tcode        = 'ZLES0106' OR
      sy-tcode        = 'ZLES0113' OR
      sy-tcode        = 'ZLES0115' OR
      sy-tcode        = 'ZLES0136' OR
      sy-tcode        = 'ZNFE'     OR "*-CS2024000086-26.09.2024-#151423-JT-inicio
      sy-tcode        = 'ZMM0079'  OR
      sy-tcode        = 'ZMM0127'  OR
*-CS2021001045 - 03.02.2022 - JT - inicio
      sy-tcode        = 'ZLES0200' OR
*-CS2021001045 - 03.02.2022 - JT - fim
*-CS2021000672 - 19.04.2022 - RJF - inicio
      sy-cprog        = 'ZMMR183'  OR
      sy-cprog        = 'ZMMR105'  OR "178025 CS2023000574 Job dinâmico PSA
      sy-cprog        = 'ZMMR0208' OR "178025 CS2023000574 Job dinâmico PSA
      sy-batch        = 'X'        OR "178025 CS2023000574 Job dinâmico PSA
*-CS2021000672 - 19.04.2022 - RJF - fim
      vg_faturamento_autom = abap_true OR "*-#133089-12.02.2024-JT
      lc_deixa_seguir = abap_true.

CHECK sy-tcode <> 'VT10'.

CHECK i_yvbpa[] IS INITIAL.

CHECK NOT i_xvttp[] IS INITIAL.

RANGES: rinicio  FOR zlest0115-dt_inicio,
        rfinal   FOR zlest0115-dt_final.

DATA : wk_vbpavb          TYPE vbpavb,
       wk_vbpa            TYPE vbpa,
       wk_adrc            TYPE adrc,
       wk_lfa1            TYPE lfa1,
       wk_zsdt0001        TYPE zsdt0001,
       wk_0001            TYPE zsdt0001,
       wk_0002            TYPE zlest0002,
       vl_ctrl            TYPE c,
       vl_opus            TYPE c,
       lva_count_reg_adto TYPE i,
       wk_agente          TYPE lfa1-lifnr,
       vl_chvadto(10)     TYPE c,
       wk_0026            TYPE zlest0026,
       wa_lips            TYPE lips,
       wa_zlest0052       TYPE zlest0052,
       it_zlest0116       TYPE TABLE OF zlest0116 WITH HEADER LINE,
       it_zlest0115       TYPE TABLE OF zlest0115 WITH HEADER LINE,
       wa_zlest0090       TYPE zlest0090,
       wa_ttds            TYPE ttds.

* Determinação do ponto de coleta
CLEAR: wk_aux, wk_vbpavb, wk_vbpa, wk_adrc, wk_0001, wk_0002, wk_lfa1, vl_ctrl, wk_zsdt0001, vl_opus.

* Informações dos items do documento de transporte.
READ TABLE i_xvttp INTO wk_aux  INDEX 1.

* Informações do cabeçalho do documento de transporte.
READ TABLE i_xvttk INTO wk_auxk INDEX 1.

* Validação do parceiro ponto de coleta no documento de transporte
READ TABLE i_xvbpa WITH KEY parvw = 'PC'
                            posnr = '000000'.

IF NOT sy-subrc IS INITIAL.

* Determinação dos parceiros da remessa atribuída ao documento de transporte
  SELECT  SINGLE *  INTO wk_vbpa
    FROM vbpa
   WHERE vbeln  = wk_aux-vbeln
     AND parvw  = 'PC'.

  IF sy-subrc IS INITIAL.

* Dados de endereço do fornecedor.
    SELECT SINGLE * INTO wk_adrc
      FROM adrc
     WHERE addrnumber = wk_vbpa-adrnr.

    MOVE:
     sy-mandt      TO wk_vbpavb-mandt,
     wk_aux-tknum  TO wk_vbpavb-vbeln,
     '000000'      TO wk_vbpavb-posnr,
     'PC'          TO wk_vbpavb-parvw,
     wk_vbpa-lifnr TO wk_vbpavb-lifnr,
     wk_vbpa-adrnr TO wk_vbpavb-adrnr,
     wk_vbpa-land1 TO wk_vbpavb-land1,
     'D'           TO wk_vbpavb-adrda,
     'I'           TO wk_vbpavb-updkz,
     wk_adrc-name1 TO wk_vbpavb-name1,
     'LI'          TO wk_vbpavb-nrart,
     '08'          TO wk_vbpavb-fehgr,
     sy-langu      TO wk_vbpavb-spras.

    APPEND wk_vbpavb TO i_xvbpa.

  ELSE.
*-#133089-21.02.2024-JT-inicio
    CASE vg_faturamento_autom.
      WHEN abap_off.
        MESSAGE s015(zles).
      WHEN abap_true.
        MESSAGE s015(zles) INTO DATA(l_mesg).
        lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wk_auxk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
    ENDCASE.
*-#133089-21.02.2024-JT-fim
  ENDIF.

ENDIF.

* Determinação do local de entrega
CLEAR: wk_vbpavb, wk_vbpa, wk_adrc, wk_0001, wk_0002, wk_lfa1, vl_ctrl, wk_zsdt0001, vl_opus.

* Validação do parceiro ponto de coleta no documento de transporte
READ TABLE i_xvbpa WITH KEY parvw = 'LR'
                            posnr = '000000'.

IF NOT sy-subrc IS INITIAL.

* Determinação dos parceiros da remessa atribuída ao documento de transporte
  SELECT SINGLE * INTO wk_vbpa
    FROM vbpa
   WHERE vbeln = wk_aux-vbeln
     AND parvw = 'LR'.

  IF sy-subrc IS INITIAL.

* Dados de endereço do fornecedor.
    SELECT SINGLE *
    INTO   wk_adrc
    FROM   adrc
    WHERE  addrnumber = wk_vbpa-adrnr.

    MOVE:
     sy-mandt        TO wk_vbpavb-mandt,
     wk_aux-tknum    TO wk_vbpavb-vbeln,
     '000000'        TO wk_vbpavb-posnr,
     'LR'            TO wk_vbpavb-parvw,
     wk_vbpa-kunnr   TO wk_vbpavb-kunnr,
     wk_vbpa-adrnr   TO wk_vbpavb-adrnr,
     wk_vbpa-land1   TO wk_vbpavb-land1,
     'D'             TO wk_vbpavb-adrda,
     'I'             TO wk_vbpavb-updkz,
     wk_adrc-name1   TO wk_vbpavb-name1,
     'LI'            TO wk_vbpavb-nrart,
     '08'            TO wk_vbpavb-fehgr,
     sy-langu        TO wk_vbpavb-spras.

    APPEND wk_vbpavb TO i_xvbpa.

  ELSE.
*-#133089-21.02.2024-JT-inicio
    CASE vg_faturamento_autom.
      WHEN abap_off.
        MESSAGE s016(zles).
      WHEN abap_true.
        MESSAGE s016(zles) INTO l_mesg.
        lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wk_auxk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
    ENDCASE.
*-#133089-21.02.2024-JT-fim
  ENDIF.

ENDIF.

IF wk_auxk-vsart IN r_vsart.

* importação dos valores customizados de adiantamento e pedagio
  IMPORT vl_chvadto TO vl_chvadto FROM MEMORY ID 'ZADTOPED'.

  IF NOT vl_chvadto IS INITIAL.

    SELECT SINGLE * INTO wk_0026
      FROM zlest0026
     WHERE seq_controle = vl_chvadto.

    IF sy-subrc IS INITIAL.
      MOVE : wk_auxk-tknum     TO wk_0026-tknum,
             wk_auxk-text1(7)  TO wk_0026-placa_cav,
             sy-datum          TO wk_0026-erdat,
             sy-uzeit          TO wk_0026-uzeit,
             sy-uname          TO wk_0026-uname.
      MODIFY zlest0026 FROM wk_0026.
      DELETE FROM zlest0026 WHERE tknum = vl_chvadto AND seq_controle = vl_chvadto.
    ENDIF.

  ELSEIF wk_auxk-id_carga IS NOT INITIAL.

    SELECT SINGLE * INTO wk_0026
      FROM zlest0026
     WHERE id_carga = wk_auxk-id_carga
       AND shtyp    = wk_auxk-shtyp
       AND abfer    = wk_auxk-abfer.

    IF sy-subrc IS INITIAL.

      SELECT SINGLE COUNT( * ) INTO lva_count_reg_adto
        FROM zlest0026
       WHERE id_carga EQ wk_auxk-id_carga
         AND shtyp    EQ wk_auxk-shtyp
         AND abfer    EQ wk_auxk-abfer.

      MOVE : wk_auxk-tknum     TO wk_0026-tknum,
             wk_auxk-text1(7)  TO wk_0026-placa_cav,
             wk_auxk-shtyp     TO wk_0026-shtyp,
             wk_auxk-abfer     TO wk_0026-abfer,
             sy-datum          TO wk_0026-erdat,
             sy-uzeit          TO wk_0026-uzeit,
             sy-uname          TO wk_0026-uname.
      DELETE FROM zlest0026 WHERE id_carga EQ wk_auxk-id_carga AND id_carga NE space.

      IF lva_count_reg_adto EQ 1.
        MODIFY zlest0026 FROM wk_0026.
      ENDIF.
    ENDIF.

  ENDIF.

  IF wk_auxk-abfer = 1 OR wk_auxk-abfer = 2  OR wk_auxk-abfer = 3 OR wk_auxk-abfer = 4 .

* Validação do parceiro agente de seguro no documento de transporte
    READ TABLE i_xvbpa WITH KEY parvw = 'SG'
                                posnr = '000000'.

    IF NOT sy-subrc IS INITIAL.

      SELECT SINGLE * FROM lips INTO wa_lips WHERE vbeln EQ wk_aux-vbeln.

      SELECT SINGLE * FROM ttds INTO wa_ttds WHERE tplst EQ wa_lips-werks.

      SELECT SINGLE * FROM zlest0052 INTO wa_zlest0052 WHERE bukrs    EQ wa_ttds-bukrs
                                                         AND matkl    EQ wa_lips-matkl
                                                         AND status   EQ 'X'
                                                         AND bloqueio EQ 'A'.
      IF sy-subrc IS INITIAL.
        wk_lfa1-lifnr = wa_zlest0052-lifnr.
      ELSE.
        SELECT * INTO TABLE it_zlest0116
          FROM zlest0116
         WHERE cd_empresa EQ wa_ttds-bukrs
           AND cd_grupo   EQ wa_lips-matkl.

        IF sy-subrc IS INITIAL.
          CLEAR: rinicio.
          rinicio-sign   = 'I'.
          rinicio-option = 'LE'.
          rinicio-low    = sy-datum.
          rinicio-high   = sy-datum.
          APPEND rinicio.

          CLEAR: rfinal.
          rfinal-sign   = 'I'.
          rfinal-option = 'GE'.
          rfinal-low    = sy-datum.
          rfinal-high   = sy-datum.
          APPEND rfinal.

          SELECT * INTO TABLE it_zlest0115
            FROM zlest0115
             FOR ALL ENTRIES IN it_zlest0116
           WHERE cd_apolice EQ it_zlest0116-cd_apolice
             AND dt_inicio     IN rinicio
             AND dt_final      IN rfinal
             AND ck_excluido   EQ space.

          IF sy-subrc IS INITIAL.
            READ TABLE it_zlest0115 INDEX 1.
            wk_lfa1-lifnr = it_zlest0115-cd_fornecedor.
          ENDIF.
        ENDIF.
      ENDIF.

      IF sy-subrc IS INITIAL.
        SELECT SINGLE *
        INTO   wk_lfa1
        FROM   lfa1
        WHERE  lifnr = wk_lfa1-lifnr.

* Dados de endereço do fornecedor.
        SELECT SINGLE *
        INTO   wk_adrc
        FROM   adrc
        WHERE  addrnumber = wk_lfa1-adrnr.

        MOVE:
         sy-mandt           TO wk_vbpavb-mandt,
         wk_aux-tknum       TO wk_vbpavb-vbeln,
         '000000'           TO wk_vbpavb-posnr,
         'SG'               TO wk_vbpavb-parvw,
         wk_lfa1-lifnr      TO wk_vbpavb-lifnr,
         wk_lfa1-adrnr      TO wk_vbpavb-adrnr,
         wk_lfa1-land1      TO wk_vbpavb-land1,
         'D'                TO wk_vbpavb-adrda,
         'I'                TO wk_vbpavb-updkz,
         wk_adrc-name1      TO wk_vbpavb-name1,
         'LI'               TO wk_vbpavb-nrart,
         '08'               TO wk_vbpavb-fehgr,
         sy-langu           TO wk_vbpavb-spras.

        APPEND wk_vbpavb TO i_xvbpa.
      ELSE.
*-#133089-21.02.2024-JT-inicio
        CASE vg_faturamento_autom.
          WHEN abap_off.
            MESSAGE s034(zles).
          WHEN abap_true.
            MESSAGE s034(zles) INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wk_auxk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
      ENDIF.
    ENDIF.
  ENDIF.

  IF wk_auxk-abfer = 1 OR wk_auxk-abfer = 2 OR wk_auxk-abfer = 3 OR wk_auxk-abfer = 4.

    CLEAR: wk_0026.

    SELECT SINGLE * INTO wk_0026
      FROM zlest0026
     WHERE tknum EQ wk_auxk-tknum.

* Validação do parceiro agente de pedágio no documento de transporte
    READ TABLE i_xvbpa WITH KEY parvw = 'PD' posnr = '000000'.

    IF ( NOT sy-subrc IS INITIAL ) AND ( wk_0026-pedagio NE 0 ).

      "Documento SD: fornecimento: dados de item
      SELECT SINGLE * FROM lips INTO wa_lips WHERE vbeln EQ wk_aux-vbeln.

      "Unidade organizacional: locais de organização do transporte
      SELECT SINGLE * FROM ttds INTO wa_ttds WHERE tplst EQ wa_lips-werks.

      CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
        EXPORTING
          centro               = wa_lips-werks
        IMPORTING
          centro_real          = wa_zlest0090-werks
        EXCEPTIONS
          informar_centro      = 1
          nao_centro_r_virtual = 2
          informar_centro_out  = 3
          informar_centro_v    = 4
          OTHERS               = 5.

      IF sy-subrc IS NOT INITIAL.
*-#133089-21.02.2024-JT-inicio
        CASE vg_faturamento_autom.
          WHEN abap_off.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          WHEN abap_true.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wk_auxk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
      ENDIF.

      "Frete Rodoviário Parceiro Agente de  Pedágio.
      IF wk_0026-tp_admim_ped IS INITIAL.
        SELECT SINGLE * FROM zlest0090 INTO wa_zlest0090 WHERE bukrs EQ wa_ttds-bukrs AND werks EQ wa_zlest0090-werks AND tp_servico EQ '1'.
      ELSE.
        SELECT SINGLE * FROM zlest0090 INTO wa_zlest0090 WHERE bukrs EQ wa_ttds-bukrs AND werks EQ wa_zlest0090-werks AND tp_adm EQ wk_0026-tp_admim_ped AND tp_servico EQ '1'.
      ENDIF.

      IF sy-subrc IS INITIAL.
        "Dados de endereço do fornecedor.
        SELECT SINGLE * INTO wk_lfa1 FROM lfa1 WHERE lifnr = wa_zlest0090-agenteped.

        "Dados de endereço do fornecedor.
        SELECT SINGLE * INTO wk_adrc FROM adrc WHERE addrnumber = wk_lfa1-adrnr.

        MOVE:
         sy-mandt               TO wk_vbpavb-mandt,
         wk_aux-tknum           TO wk_vbpavb-vbeln,
         '000000'               TO wk_vbpavb-posnr,
         'PD'                   TO wk_vbpavb-parvw,
         wa_zlest0090-agenteped TO wk_vbpavb-lifnr,
         wk_lfa1-adrnr          TO wk_vbpavb-adrnr,
         wk_lfa1-land1          TO wk_vbpavb-land1,
         'D'                    TO wk_vbpavb-adrda,
         'I'                    TO wk_vbpavb-updkz,
         wk_adrc-name1          TO wk_vbpavb-name1,
         'LI'                   TO wk_vbpavb-nrart,
         '08'                   TO wk_vbpavb-fehgr,
         sy-langu               TO wk_vbpavb-spras.
        APPEND wk_vbpavb        TO i_xvbpa.
      ELSE.
*-#133089-21.02.2024-JT-inicio
        CASE vg_faturamento_autom.
          WHEN abap_off.
            MESSAGE s035(zles).
          WHEN abap_true.
            MESSAGE s035(zles) INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wk_auxk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
      ENDIF.

    ELSEIF ( sy-subrc IS INITIAL ) AND ( wk_0026-pedagio EQ 0 ).
      DELETE i_xvbpa INDEX sy-tabix.
    ENDIF.

  ENDIF.

* Determinação do proprietário do veículo
  CLEAR: wk_vbpavb, wk_vbpa, wk_adrc, wk_0001, wk_0002, wk_lfa1, vl_ctrl, wk_zsdt0001, vl_opus.

* Controle Opus
  IF wk_auxk-abfer = 1 OR wk_auxk-abfer = 3.
    SELECT SINGLE * INTO wk_zsdt0001 FROM zsdt0001 WHERE doc_rem = wk_aux-vbeln.
    IF sy-subrc IS INITIAL.
      vl_opus = 'X'.
    ENDIF.
  ELSE.
    vl_opus = 'X'.
  ENDIF.

* Validação do parceiro proprietário do veículo no documento de transporte
  READ TABLE i_xvbpa WITH KEY parvw = 'PV'
                              posnr = '000000'.

  IF NOT sy-subrc IS INITIAL.

    IF wk_auxk-abfer = 1 OR wk_auxk-abfer = 3.

      "Determinação dos parceiros da remessa atribuída ao documento de transporte
      SELECT SINGLE * INTO wk_0001 FROM zsdt0001 WHERE doc_rem = wk_aux-vbeln AND tp_movimento = 'S'.

*-CS2021001045 - 03.02.2022 - JT - inicio
      CLEAR: wk_zlest0108.

      IF wk_0001 IS INITIAL.
        SELECT SINGLE * INTO wk_zlest0108 FROM zlest0108 WHERE vbeln = wk_aux-vbeln.
        IF sy-subrc IS NOT INITIAL.
          SELECT SINGLE * INTO wk_zlest0108 FROM zlest0108 WHERE doc_transp = wk_aux-tknum.
        ENDIF.
        IF sy-subrc IS NOT INITIAL.
          SELECT SINGLE * INTO CORRESPONDING FIELDS OF wk_zlest0108 FROM zlest0211 WHERE vbeln = wk_aux-vbeln.
        ENDIF.
      ENDIF.
*-CS2021001045 - 03.02.2022 - JT - fim
    ELSE.

      "Determinação dos parceiros da remessa atribuída ao documento de transporte
      SELECT SINGLE * INTO wk_0001 FROM zsdt0001 WHERE doc_rem = wk_aux-vbeln AND tp_movimento = 'E'.

      IF sy-subrc NE 0.
        "Determinação dos parceiros do AVISO atribuída ao documento de transporte
        SELECT SINGLE * INTO wk_0001 FROM zsdt0001 WHERE doc_aviso = wk_aux-vbeln AND tp_movimento = 'S'.
      ENDIF.

      CLEAR: wk_zlest0108.

      IF wk_0001 IS INITIAL.
        SELECT SINGLE * INTO wk_zlest0108 FROM zlest0108 WHERE vbeln = wk_aux-vbeln.
        IF sy-subrc IS NOT INITIAL.
          SELECT SINGLE * INTO wk_zlest0108 FROM zlest0108 WHERE doc_transp = wk_aux-tknum.
        ENDIF.
*-CS2021001045 - 03.02.2022 - JT - inicio
        IF sy-subrc IS NOT INITIAL.
          SELECT SINGLE * INTO CORRESPONDING FIELDS OF wk_zlest0108 FROM zlest0211 WHERE vbeln = wk_aux-vbeln.
        ENDIF.
*-CS2021001045 - 03.02.2022 - JT - fim
      ENDIF.
    ENDIF.

    IF wk_0001 IS INITIAL AND wk_auxk-id_romaneio IS NOT INITIAL.
      SELECT SINGLE * INTO wk_0001
        FROM zsdt0001
       WHERE ch_referencia EQ wk_auxk-id_romaneio.
    ENDIF.

    IF ( NOT wk_0001 IS INITIAL ) OR ( wk_zlest0108 IS NOT INITIAL ).

*-CS2021001045 - 03.02.2022 - JT - inicio
      IF vl_opus = 'X' OR wk_auxk-shtyp = 'Z020' OR wk_auxk-shtyp = 'Z005' .
*-CS2021001045 - 03.02.2022 - JT - fim

        IF wk_0001 IS NOT INITIAL.
          SELECT SINGLE * INTO wk_0002 FROM zlest0002 WHERE pc_veiculo  = wk_0001-placa_cav AND st_bloqueio <> 'X'.
        ENDIF.

        IF wk_zlest0108 IS NOT INITIAL.
          SELECT SINGLE * INTO wk_0002 FROM zlest0002 WHERE pc_veiculo  = wk_zlest0108-placa_cav AND st_bloqueio <> 'X'.
        ENDIF.


        IF sy-subrc IS INITIAL.

* Determinação dos parceiros da remessa atribuída ao documento de transporte
          SELECT  SINGLE       *
          INTO                 wk_lfa1
          FROM                 lfa1
          WHERE                lifnr = wk_0002-proprietario.

          IF sy-subrc IS INITIAL.

* Grupo de contas do parceiro proprietário do veículo
*            CASE wk_lfa1-ktokk.
*              WHEN 'ZFFF'.
*              WHEN 'ZFFJ'.
*              WHEN 'ZFIC'.
*              WHEN OTHERS.
*                vl_ctrl = 'X'.
*                MESSAGE s011(zles).
*            ENDCASE.

            IF vl_ctrl IS INITIAL.

* Dados de endereço do fornecedor.
              SELECT SINGLE *
              INTO   wk_adrc
              FROM   adrc
              WHERE  addrnumber = wk_lfa1-adrnr.

              MOVE:
               sy-mandt        TO wk_vbpavb-mandt,
               wk_aux-tknum    TO wk_vbpavb-vbeln,
               '000000'        TO wk_vbpavb-posnr,
               'PV'            TO wk_vbpavb-parvw,
               wk_lfa1-lifnr   TO wk_vbpavb-lifnr,
               wk_lfa1-adrnr   TO wk_vbpavb-adrnr,
               wk_lfa1-land1   TO wk_vbpavb-land1,
               'D'             TO wk_vbpavb-adrda,
               'I'             TO wk_vbpavb-updkz,
               wk_adrc-name1   TO wk_vbpavb-name1,
               'LI'            TO wk_vbpavb-nrart,
               '08'            TO wk_vbpavb-fehgr,
               sy-langu        TO wk_vbpavb-spras.

              APPEND wk_vbpavb TO i_xvbpa.

            ENDIF.

          ELSE.
*-#133089-21.02.2024-JT-inicio
            CASE vg_faturamento_autom.
              WHEN abap_off.
                MESSAGE s029(zles).
              WHEN abap_true.
                MESSAGE s029(zles) INTO l_mesg.
                lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wk_auxk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
            ENDCASE.
*-#133089-21.02.2024-JT-fim
          ENDIF.
        ELSE.
*-#133089-21.02.2024-JT-inicio
          CASE vg_faturamento_autom.
            WHEN abap_off.
              MESSAGE s008(zles).
            WHEN abap_true.
              MESSAGE s008(zles) INTO l_mesg.
              lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wk_auxk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
          ENDCASE.
*-#133089-21.02.2024-JT-fim
        ENDIF.

      ELSE.

        IF vl_opus = 'X'.
*-#133089-21.02.2024-JT-inicio
          CASE vg_faturamento_autom.
            WHEN abap_off.
              MESSAGE s028(zles).
            WHEN abap_true.
              MESSAGE s028(zles) INTO l_mesg.
              lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wk_auxk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
          ENDCASE.
*-#133089-21.02.2024-JT-fim
        ELSE.
*      MESSAGE s007(zles).
        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.

* Determinação do motorista do veículo
  CLEAR: wk_vbpavb, wk_vbpa, wk_adrc, wk_0001, wk_0002, wk_lfa1, vl_ctrl, vl_opus.

* Controle Opus
  IF wk_auxk-abfer = 1 OR wk_auxk-abfer = 3.

    SELECT SINGLE * INTO wk_zsdt0001 FROM zsdt0001 WHERE doc_rem = wk_aux-vbeln.
    IF sy-subrc IS INITIAL.
      vl_opus = 'X'.
    ENDIF.

  ELSE.
    vl_opus = 'X'.
  ENDIF.

* Validação do parceiro motorista do veículo no documento de transporte
  READ TABLE i_xvbpa WITH KEY parvw = 'MT'
                              posnr = '000000'.

  IF NOT sy-subrc IS INITIAL.

    IF wk_auxk-abfer = 1 OR wk_auxk-abfer = 3.

* Determinação dos parceiros da remessa atribuída ao documento de transporte
      SELECT  SINGLE       *
      INTO                 wk_0001
      FROM                 zsdt0001
      WHERE                doc_rem      = wk_aux-vbeln AND
                           tp_movimento = 'S'.

*-CS2021001045 - 03.02.2022 - JT - inicio
      CLEAR: wk_zlest0108.
      IF wk_0001 IS INITIAL.
        SELECT  SINGLE     *
        INTO               wk_zlest0108
        FROM               zlest0108
        WHERE              vbeln      = wk_aux-vbeln.
      ENDIF.

      IF wk_zlest0108 IS INITIAL.
        SELECT SINGLE *
          INTO wk_zlest0108
          FROM zlest0108
         WHERE doc_transp = wk_auxk-tknum.
      ENDIF.

      IF wk_zlest0108 IS INITIAL.
        SELECT  SINGLE     *
        INTO CORRESPONDING FIELDS OF  wk_zlest0108
        FROM               zlest0211
        WHERE              vbeln      = wk_aux-vbeln.
      ENDIF.
*-CS2021001045 - 03.02.2022 - JT - inicio
    ELSE.

* Determinação dos parceiros da remessa atribuída ao documento de transporte
      SELECT  SINGLE       *
      INTO                 wk_0001
      FROM                 zsdt0001
      WHERE                doc_rem      = wk_aux-vbeln AND
                           tp_movimento = 'E'.
      IF sy-subrc NE 0.
        SELECT  SINGLE       *
        INTO                 wk_0001
        FROM                 zsdt0001
        WHERE                doc_aviso      = wk_aux-vbeln AND
                             tp_movimento = 'S'.
      ENDIF.
      CLEAR: wk_zlest0108.
      IF wk_0001 IS INITIAL.
        SELECT  SINGLE     *
        INTO               wk_zlest0108
        FROM               zlest0108
        WHERE              vbeln      = wk_aux-vbeln.
      ENDIF.

      IF wk_zlest0108 IS INITIAL.
        SELECT SINGLE *
          INTO wk_zlest0108
          FROM zlest0108
         WHERE doc_transp = wk_auxk-tknum.
      ENDIF.

*-CS2021001045 - 03.02.2022 - JT - inicio
      IF wk_zlest0108 IS INITIAL.
        SELECT  SINGLE     *
        INTO CORRESPONDING FIELDS OF  wk_zlest0108
        FROM               zlest0211
        WHERE              vbeln      = wk_aux-vbeln.
      ENDIF.
*-CS2021001045 - 03.02.2022 - JT - inicio

    ENDIF.

    IF wk_0001 IS INITIAL AND wk_auxk-id_romaneio IS NOT INITIAL.
      SELECT SINGLE * INTO wk_0001
        FROM zsdt0001
       WHERE ch_referencia EQ wk_auxk-id_romaneio.
    ENDIF.

    IF ( NOT wk_0001 IS INITIAL ) OR ( wk_zlest0108 IS NOT INITIAL ) .

*-CS2021001045 - 03.02.2022 - JT - inicio
      IF vl_opus = 'X' OR wk_auxk-shtyp = 'Z020' OR wk_auxk-shtyp = 'Z005'.
*-CS2021001045 - 03.02.2022 - JT - fim

* Determinação dos parceiros da remessa atribuída ao documento de transporte
        IF ( wk_0001 IS NOT INITIAL ).

          SELECT  SINGLE       *
          INTO                 wk_lfa1
          FROM                 lfa1
          WHERE                lifnr = wk_0001-motorista.

        ENDIF.

        IF ( wk_zlest0108 IS NOT INITIAL ).

          SELECT  SINGLE       *
          INTO                 wk_lfa1
          FROM                 lfa1
          WHERE                lifnr = wk_zlest0108-motorista.

        ENDIF.


        IF sy-subrc IS INITIAL.

* Grupo de contas do parceiro motorista do veículo
          CASE wk_lfa1-ktokk.
            WHEN 'ZMOT'.
            WHEN OTHERS.
              vl_ctrl = 'X'.
*-#133089-21.02.2024-JT-inicio
              CASE vg_faturamento_autom.
                WHEN abap_off.
                  MESSAGE s030(zles).
                WHEN abap_true.
                  MESSAGE s030(zles) INTO l_mesg.
                  lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wk_auxk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
              ENDCASE.
*-#133089-21.02.2024-JT-fim

          ENDCASE.

          IF vl_ctrl IS INITIAL.

* Dados de endereço do fornecedor.
            SELECT SINGLE *
            INTO   wk_adrc
            FROM   adrc
            WHERE  addrnumber = wk_lfa1-adrnr.

            MOVE:
             sy-mandt        TO wk_vbpavb-mandt,
             wk_aux-tknum    TO wk_vbpavb-vbeln,
             '000000'        TO wk_vbpavb-posnr,
             'MT'            TO wk_vbpavb-parvw,
             wk_lfa1-lifnr   TO wk_vbpavb-lifnr,
             wk_lfa1-adrnr   TO wk_vbpavb-adrnr,
             wk_lfa1-land1   TO wk_vbpavb-land1,
             'D'             TO wk_vbpavb-adrda,
             'I'             TO wk_vbpavb-updkz,
             wk_adrc-name1   TO wk_vbpavb-name1,
             'LI'            TO wk_vbpavb-nrart,
             '08'            TO wk_vbpavb-fehgr,
             sy-langu        TO wk_vbpavb-spras.

            APPEND wk_vbpavb TO i_xvbpa.

          ENDIF.

        ELSE.
*-#133089-21.02.2024-JT-inicio
          CASE vg_faturamento_autom.
            WHEN abap_off.
              MESSAGE s013(zles).
            WHEN abap_true.
              MESSAGE s013(zles) INTO l_mesg.
              lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wk_auxk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
          ENDCASE.
*-#133089-21.02.2024-JT-fim
        ENDIF.

      ELSE.
        IF vl_opus = 'X'.
*-#133089-21.02.2024-JT-inicio
          CASE vg_faturamento_autom.
            WHEN abap_off.
              MESSAGE s028(zles).
            WHEN abap_true.
              MESSAGE s028(zles) INTO l_mesg.
              lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wk_auxk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
          ENDCASE.
*-#133089-21.02.2024-JT-fim
        ELSE.
*-#133089-21.02.2024-JT-inicio
          CASE vg_faturamento_autom.
            WHEN abap_off.
              MESSAGE s010(zles).
              EXIT.
            WHEN abap_true.
              MESSAGE s010(zles) INTO l_mesg.
              lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wk_auxk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
          ENDCASE.
*-#133089-21.02.2024-JT-fim
        ENDIF.
      ENDIF.

    ENDIF.

  ENDIF.



* Ajuste do Parceiro RM - 14.03.2013 - Victor Hugo - Inicio
  IF ( sy-tcode EQ 'VT01N' ) AND ( wk_auxk-shtyp = 'Z026' ).

    CLEAR: wk_vbpa, wk_adrc.

* Validação do parceiro ponto de coleta no documento de transporte
    READ TABLE i_xvbpa WITH KEY parvw = 'RM'
                                posnr = '000000'.

    IF NOT sy-subrc IS INITIAL.

* Determinação dos parceiros da remessa atribuída ao documento de transporte
      SELECT  SINGLE *  INTO wk_vbpa
        FROM vbpa
       WHERE vbeln  = wk_aux-vbeln
         AND parvw  = 'RM'.

      IF sy-subrc IS INITIAL.

* Dados de endereço do fornecedor.
        SELECT SINGLE * INTO wk_adrc
          FROM adrc
         WHERE addrnumber = wk_vbpa-adrnr.

        MOVE:
         sy-mandt      TO wk_vbpavb-mandt,
         wk_aux-tknum  TO wk_vbpavb-vbeln,
         '000000'      TO wk_vbpavb-posnr,
         'RM'          TO wk_vbpavb-parvw,
         wk_vbpa-lifnr TO wk_vbpavb-lifnr,
         wk_vbpa-adrnr TO wk_vbpavb-adrnr,
         wk_vbpa-land1 TO wk_vbpavb-land1,
         'D'           TO wk_vbpavb-adrda,
         'I'           TO wk_vbpavb-updkz,
         wk_adrc-name1 TO wk_vbpavb-name1,
         'LI'          TO wk_vbpavb-nrart,
         '08'          TO wk_vbpavb-fehgr,
         sy-langu      TO wk_vbpavb-spras.

        APPEND wk_vbpavb TO i_xvbpa.

      ELSE.
*-#133089-21.02.2024-JT-inicio
        CASE vg_faturamento_autom.
          WHEN abap_off.
            MESSAGE s015(zles).
          WHEN abap_true.
            MESSAGE s015(zles) INTO l_mesg.
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wk_auxk-id_romaneio i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
        ENDCASE.
*-#133089-21.02.2024-JT-fim
      ENDIF.
    ENDIF.
  ENDIF.
* Ajuste do Parceiro RM - 14.03.2013 - Victor Hugo - Fim

  CALL FUNCTION 'Z_LES_DETERMINA_PARCEIRO_AVISO'
    TABLES
      i_xvttk = i_xvttk
      i_xvttp = i_xvttp
      i_xvbpa = i_xvbpa.

ENDIF.
