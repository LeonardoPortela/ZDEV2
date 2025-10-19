FUNCTION z_ck_pedagio_mesmo_veiculo.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_ZLEST0090) TYPE  ZLEST0090
*"     REFERENCE(I_ZLEST0026) TYPE  ZLEST0026 OPTIONAL
*"     REFERENCE(I_ZLEST0122) TYPE  ZLEST0122 OPTIONAL
*"     REFERENCE(I_TKNUM) TYPE  TKNUM OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_MESMO_VEICULO) TYPE  CHAR01
*"     REFERENCE(E_MESMA_CARGA) TYPE  CHAR01
*"  CHANGING
*"     VALUE(M_CK_CREDITA_PED) TYPE  ZDE_CRED_PEDAGIO OPTIONAL
*"----------------------------------------------------------------------
  RANGES: r_dt_base            FOR zlest0123-dt_emissao_pedagio,
          r_id_rota_repom      FOR zlest0122-id_rota_repom,
          r_id_percurso_repom  FOR zlest0122-id_percurso_repom,
          r_id_rota            FOR zlest0026-id_rota.

  DATA: it_zlest0123_anterior TYPE TABLE OF zlest0123 WITH HEADER LINE,
        it_zlest0026_anterior TYPE TABLE OF zlest0026 WITH HEADER LINE,
        it_vttp               TYPE TABLE OF vttp WITH HEADER LINE,
        it_lips               TYPE TABLE OF lips WITH HEADER LINE,
        it_mara               TYPE TABLE OF mara WITH HEADER LINE,
        it_romaneios          TYPE zsdt0001_t,
        wl_rom_atual          TYPE zsdt0001.

  DATA: v_algodao     TYPE c,
        v_dt_lim      TYPE erdat,
        v_werks_atual TYPE lips-werks.

  CLEAR: it_zlest0123_anterior[], it_zlest0026_anterior[], r_dt_base[],
         v_algodao, v_werks_atual, wl_rom_atual, it_romaneios[].

  e_mesmo_veiculo = abap_false.
  e_mesma_carga   = abap_false.

  CHECK i_tknum IS NOT INITIAL.

  SELECT *
    FROM vttp INTO TABLE it_vttp
   WHERE tknum EQ i_tknum.

  CHECK it_vttp[] IS NOT INITIAL.

  SELECT *
    FROM lips INTO TABLE it_lips
     FOR ALL ENTRIES IN it_vttp
   WHERE vbeln EQ it_vttp-vbeln.

  IF it_lips[] IS NOT INITIAL.
    SELECT *
      FROM mara INTO TABLE it_mara
       FOR ALL ENTRIES IN it_lips
     WHERE matnr EQ it_lips-matnr.

    LOOP AT it_mara WHERE matkl EQ  '700140'.
      v_algodao = abap_true.
      EXIT.
    ENDLOOP.
  ENDIF.

  "Determinar Filial Tknum Atual
  LOOP AT it_lips.
    v_werks_atual = it_lips-werks.

    SELECT SINGLE *
      FROM zsdt0001 INTO wl_rom_atual
     WHERE doc_rem = it_lips-vbeln.

    EXIT.
  ENDLOOP.

  IF wl_rom_atual IS NOT INITIAL.
    CALL METHOD zcl_romaneio=>get_ck_faturar
      EXPORTING
        i_ch_referencia_sai = wl_rom_atual-ch_referencia
      IMPORTING
        e_romaneios         = it_romaneios.
  ENDIF.

  "Definir Data Base
  r_dt_base-sign   = 'I'.

  IF v_algodao EQ abap_true.
    r_dt_base-option =  'GE'.
    r_dt_base-low    =  sy-datum - 3.
  ELSE.
    r_dt_base-option =  'GE'.
    r_dt_base-low    =  sy-datum - 1.
  ENDIF.

  APPEND r_dt_base.

  IF ( i_zlest0090-tp_adm EQ '09' ) OR  "(TipFrete) UNIK S.A.
     ( i_zlest0090-tp_op  EQ 'M'  ). "Manual

    CLEAR: r_id_rota[].

    IF ( v_algodao EQ abap_false ) AND ( i_zlest0026-id_rota IS NOT INITIAL ).
      r_id_rota-sign   = 'I'.
      r_id_rota-option = 'EQ'.
      r_id_rota-low    = i_zlest0026-id_rota.
      APPEND r_id_rota.
    ENDIF.

    SELECT * INTO TABLE it_zlest0026_anterior
      FROM zlest0026 AS z
     WHERE z~placa_cav EQ i_zlest0026-placa_cav
       AND z~erdat     IN r_dt_base
       AND z~id_rota   IN r_id_rota
       AND EXISTS ( SELECT * FROM vttk AS t WHERE t~tknum EQ z~tknum AND t~tknum NE i_tknum ).

    CHECK it_zlest0026_anterior[] IS NOT INITIAL.

    "Checar se ultimo Pedagio para o Veiculo, é de uma filial diferente.
    SORT it_zlest0026_anterior BY erdat DESCENDING.
    LOOP AT it_zlest0026_anterior.

      SELECT SINGLE *
        FROM vttp INTO @DATA(_wl_vttp_ant)
       WHERE tknum EQ @it_zlest0026_anterior-tknum.

      IF sy-subrc EQ 0.

        "Checar se é a mesma Carga
        SELECT SINGLE *
          FROM zsdt0001 INTO @DATA(wl_zsdt0001_ant)
         WHERE doc_rem EQ @_wl_vttp_ant-vbeln.

        IF sy-subrc EQ 0.
          READ TABLE it_romaneios INTO DATA(_wl_rom) WITH KEY ch_referencia = wl_zsdt0001_ant-ch_referencia.
          IF sy-subrc EQ 0.
            e_mesmo_veiculo = abap_true.
            e_mesma_carga   = abap_true.
            RETURN.
          ENDIF.
        ENDIF.

        v_dt_lim = sy-datum - 1.
        IF it_zlest0026_anterior-erdat GE v_dt_lim.
          e_mesmo_veiculo  = abap_true.
          m_ck_credita_ped = abap_false.  "*-#146705-25.07.2024-JT-#146705
          RETURN.
        ENDIF.

        SELECT SINGLE werks
          FROM lips INTO @DATA(v_werks_ant)
         WHERE vbeln EQ @_wl_vttp_ant-vbeln.

        IF ( sy-subrc EQ 0 ) AND ( v_werks_ant NE v_werks_atual ).
          e_mesmo_veiculo  = abap_true.
          m_ck_credita_ped = abap_false.  "*-#146705-25.07.2024-JT-#146705
          RETURN.
        ENDIF.

      ENDIF.

      RETURN.
    ENDLOOP.


  ELSEIF i_zlest0090-tp_adm EQ '03'. "REPOM S.A.

    CLEAR: r_id_rota_repom[], r_id_percurso_repom[].

    IF v_algodao EQ abap_false.
      r_id_rota_repom-sign   = 'I'.
      r_id_rota_repom-option = 'EQ'.
      r_id_rota_repom-low    = i_zlest0122-id_rota_repom.
      APPEND r_id_rota_repom.

      r_id_percurso_repom-sign   = 'I'.
      r_id_percurso_repom-option = 'EQ'.
      r_id_percurso_repom-low    = i_zlest0122-id_percurso_repom.
      APPEND r_id_percurso_repom.
    ENDIF.

    SELECT * INTO TABLE it_zlest0123_anterior
      FROM zlest0123
     WHERE veiculo_placa      EQ i_zlest0026-placa_cav
       AND dt_emissao_pedagio IN r_dt_base
       AND id_rota_repom      IN r_id_rota_repom
       AND id_percurso_repom  IN r_id_percurso_repom
       AND tknum              NE i_tknum
       AND tp_status_aut      EQ '3'
       AND tp_status_can      NE '3'.

    CHECK it_zlest0123_anterior[] IS NOT INITIAL.

    "Checar se ultimo Pedagio para o Veiculo
    SORT it_zlest0123_anterior BY dt_emissao_pedagio DESCENDING.
    LOOP AT it_zlest0123_anterior.

      SELECT SINGLE *
        FROM vttp INTO _wl_vttp_ant
       WHERE tknum EQ it_zlest0123_anterior-tknum.

      IF sy-subrc EQ 0.

        "Checar se é a mesma Carga
        SELECT SINGLE *
          FROM zsdt0001 INTO wl_zsdt0001_ant
         WHERE doc_rem EQ _wl_vttp_ant-vbeln.

        IF sy-subrc EQ 0.
          READ TABLE it_romaneios INTO _wl_rom WITH KEY ch_referencia = wl_zsdt0001_ant-ch_referencia.
          IF sy-subrc EQ 0.
            e_mesmo_veiculo = abap_true.
            e_mesma_carga   = abap_true.
            RETURN.
          ENDIF.
        ENDIF.

        v_dt_lim = sy-datum - 1.
        IF it_zlest0123_anterior-dt_emissao_pedagio GE v_dt_lim.
          e_mesmo_veiculo = abap_true.
          RETURN.
        ENDIF.

        SELECT SINGLE werks
          FROM lips INTO v_werks_ant
         WHERE vbeln EQ _wl_vttp_ant-vbeln.

        IF ( sy-subrc EQ 0 ) AND ( v_werks_ant NE v_werks_atual ).
          e_mesmo_veiculo = abap_true.
          RETURN.
        ENDIF.

      ENDIF.

      RETURN.
    ENDLOOP.

  ENDIF.


ENDFUNCTION.
