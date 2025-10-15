*&---------------------------------------------------------------------*
*&  Include           ZLESR0162_F01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                     Controle de Alterações                           *
*----------------------------------------------------------------------*
* Data      |Request   |Autor       |Alteração                         *
*----------------------------------------------------------------------*
* 19/08/2025|DEVK9A2R4U|NSEGATIN    |Exibir Campos Latitude e Longitude*
*                                   |Chamado: 187200.                  *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_preenche_campos .

  SET PARAMETER ID 'COD_CLI'  FIELD space.
  SET PARAMETER ID 'COD_PC'   FIELD space.
  SET PARAMETER ID 'ZONA_DES' FIELD space.
  SET PARAMETER ID 'ZONA_ORI' FIELD space.

  PERFORM f_busca_dados_cliente.

  PERFORM f_busca_dados_pc.

  PERFORM f_calc_duracao_transito.

  PERFORM f_busca_itinerario.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_DADOS_CLIENTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_busca_dados_cliente .

  wa_kna1-kunnr = |{ wa_kna1-kunnr ALPHA = IN }|.
  v_kunnr = |{ v_kunnr ALPHA = IN }|.

  IF ( ( wa_kna1-kunnr IS NOT INITIAL ) AND wa_kna1-lzone IS INITIAL ) OR wa_kna1-kunnr <> v_kunnr.

    wa_kna1-kunnr = |{ wa_kna1-kunnr ALPHA = IN }|.

    v_kunnr = wa_kna1-kunnr.

    CLEAR wa_kna1-name1.
    IF lv_zone_des IS NOT INITIAL.
      SELECT SINGLE kunnr, land1, name1, stras, ort01, regio, stcd1, stcd3
          FROM kna1 INTO @DATA(w_kna11) WHERE kunnr = @wa_kna1-kunnr AND land1 EQ 'BR'.
      IF ( sy-subrc = 0 ).

        wa_kna1     = CORRESPONDING #( w_kna11 ).
        wa_kna1-lzone = lv_zone_des.
        CLEAR lv_zone_des.

        IF ( wa_kna1-lzone IS NOT INITIAL ).
**<<<------"187200 - NMS - INI------>>>
          SELECT SINGLE zlatitude zlongitude FROM tzone
            INTO ( wa_kna1-zlatitude, wa_kna1-zlongitude )
          WHERE land1 EQ wa_kna1-land1 AND zone1 EQ wa_kna1-lzone.
**<<<------"187200 - NMS - FIM------>>>
          SELECT SINGLE vtext FROM tzont INTO wa_kna1-zone_desc
            WHERE spras EQ 'P' AND land1 EQ 'BR' AND zone1 EQ wa_kna1-lzone.

        ENDIF.

      ENDIF.
    ELSE.

      SELECT SINGLE kunnr, land1, name1, stras, ort01, regio, stcd1, stcd3, lzone
        FROM kna1 INTO @DATA(w_kna1) WHERE kunnr = @wa_kna1-kunnr AND land1 EQ 'BR'.

      IF ( sy-subrc = 0 ).

        wa_kna1     = CORRESPONDING #( w_kna1 ).

        IF ( wa_kna1-lzone IS NOT INITIAL ).
**<<<------"187200 - NMS - INI------>>>
          SELECT SINGLE zlatitude zlongitude FROM tzone
            INTO ( wa_kna1-zlatitude, wa_kna1-zlongitude )
          WHERE land1 EQ wa_kna1-land1 AND zone1 EQ wa_kna1-lzone.
**<<<------"187200 - NMS - FIM------>>>
          SELECT SINGLE vtext FROM tzont INTO wa_kna1-zone_desc
            WHERE spras EQ 'P' AND land1 EQ 'BR' AND zone1 EQ wa_kna1-lzone.

        ENDIF.
      ELSE.

        CLEAR:wa_kna1-name1,
        wa_kna1-land1,
        wa_kna1-stras,
        wa_kna1-ort01,
        wa_kna1-regio,
        wa_kna1-stcd1,
        wa_kna1-stcd3,
        wa_kna1-lzone,
        wa_kna1-zone_desc,
        wa_trolz-route,
       wa_trolz-desc.

      ENDIF.
    ENDIF.


    SELECT SINGLE @abap_true
      INTO @DATA(lv_exists)
      FROM kna1
      WHERE kunnr EQ @wa_kna1-kunnr
       AND loevm EQ  'X'.

    IF sy-subrc EQ 4.
      SELECT SINGLE @abap_true
        INTO @lv_exists
        FROM kna1
        WHERE kunnr EQ @wa_kna1-kunnr
         AND sperr EQ  'X'.
    ENDIF.
    IF lv_exists = abap_true .
      CLEAR: wa_kna1.

      MESSAGE TEXT-e01 TYPE 'I'.

    ELSE.
      wa_kna1-kunnr = |{ wa_kna1-kunnr ALPHA = OUT }|.

    ENDIF.

  ELSE.

    IF wa_kna1-kunnr IS INITIAL.
      CLEAR: wa_kna1,
             wa_trolz.
    ENDIF.


  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_DADOS_PC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_busca_dados_pc .

  wa_lfa1_pc-lifnr = |{  wa_lfa1_pc-lifnr ALPHA = IN }|.
  v_lifnr = |{ v_lifnr ALPHA = IN }|.

  IF ( (  wa_lfa1_pc-lifnr IS NOT INITIAL ) AND wa_lfa1_pc-lzone IS INITIAL ) OR wa_lfa1_pc-lifnr <> v_lifnr..

    wa_lfa1_pc-lifnr = |{  wa_lfa1_pc-lifnr ALPHA = IN }|.

    v_lifnr = wa_lfa1_pc-lifnr.

    IF lv_zone_ori IS NOT INITIAL .
      SELECT SINGLE lifnr, land1, name1, stras, ort01, regio, stcd1, stcd3
          FROM lfa1 INTO @DATA(w_lfa11) WHERE lifnr = @wa_lfa1_pc-lifnr AND land1 EQ 'BR'.

      IF ( sy-subrc = 0 ).

        wa_lfa1_pc      = CORRESPONDING #( w_lfa11 ).
        wa_lfa1_pc-lzone = lv_zone_ori.

        CLEAR lv_zone_ori.

        IF ( wa_lfa1_pc-stcd1 IS INITIAL ).
          SELECT SINGLE stcd2 FROM lfa1 INTO wa_lfa1_pc-stcd1
            WHERE lifnr EQ wa_lfa1_pc-lifnr AND land1 EQ 'BR'.
        ENDIF.

        IF ( wa_lfa1_pc-lzone IS NOT INITIAL ).
**<<<------"187200 - NMS - INI------>>>
          SELECT SINGLE zlatitude zlongitude FROM tzone
            INTO ( wa_lfa1_pc-zlatitude, wa_lfa1_pc-zlongitude )
          WHERE land1 EQ wa_lfa1_pc-land1 AND zone1 EQ wa_lfa1_pc-lzone.
**<<<------"187200 - NMS - FIM------>>>
          SELECT SINGLE vtext FROM tzont INTO wa_lfa1_pc-zone_desc
            WHERE spras EQ 'P' AND land1 EQ 'BR' AND zone1 EQ wa_lfa1_pc-lzone.
        ENDIF.

      ENDIF.

    ELSE.

      SELECT SINGLE lifnr, land1, name1, stras, ort01, regio, stcd1, stcd3, lzone
        FROM lfa1 INTO @DATA(w_lfa1) WHERE lifnr = @wa_lfa1_pc-lifnr AND land1 EQ 'BR'.

      IF ( sy-subrc = 0 ).

        wa_lfa1_pc      = CORRESPONDING #( w_lfa1 ).

        IF ( wa_lfa1_pc-stcd1 IS INITIAL ).
          SELECT SINGLE stcd2 FROM lfa1 INTO wa_lfa1_pc-stcd1
            WHERE lifnr EQ wa_lfa1_pc-lifnr AND land1 EQ 'BR'.
        ENDIF.

        IF ( wa_lfa1_pc-lzone IS NOT INITIAL ).
**<<<------"187200 - NMS - INI------>>>
          SELECT SINGLE zlatitude zlongitude FROM tzone
            INTO ( wa_lfa1_pc-zlatitude, wa_lfa1_pc-zlongitude )
          WHERE land1 EQ wa_lfa1_pc-land1 AND zone1 EQ wa_lfa1_pc-lzone.
**<<<------"187200 - NMS - FIM------>>>
          SELECT SINGLE vtext FROM tzont INTO wa_lfa1_pc-zone_desc
            WHERE spras EQ 'P' AND land1 EQ 'BR' AND zone1 EQ wa_lfa1_pc-lzone.
        ENDIF.
      ELSE.

        CLEAR: wa_lfa1_pc-land1,
               wa_lfa1_pc-lzone,
               wa_lfa1_pc-name1,
               wa_lfa1_pc-ort01,
               wa_lfa1_pc-regio,
               wa_lfa1_pc-stcd1,
               wa_lfa1_pc-stcd3,
               wa_lfa1_pc-stras,
               wa_lfa1_pc-zone_desc,
               wa_trolz-route,
               wa_trolz-desc.

      ENDIF.
    ENDIF.

    SELECT SINGLE 'X'
      INTO @DATA(lv_exists)
      FROM lfa1
      WHERE lifnr EQ @w_lfa1-lifnr
       AND loevm EQ  'X'.
    "OR SPERR EQ 'X'.

    IF sy-subrc EQ 4.

      SELECT SINGLE 'X'
        INTO @lv_exists
        FROM lfa1
        WHERE lifnr EQ @w_lfa1-lifnr
        AND sperr EQ  'X'.
      "OR SPERR EQ 'X'.

    ENDIF.
    IF lv_exists = abap_true .
      MESSAGE TEXT-e01 TYPE 'I'.
      CLEAR: wa_lfa1_pc.
*      vg_focus = 'WA_LFA1_PC-LIFNR'.
    ELSE.
*      vg_focus = 'WA_KNA1-KUNNR'.
      wa_lfa1_pc-lifnr = |{ wa_lfa1_pc-lifnr ALPHA = OUT }|.
    ENDIF.

  ELSE.

    IF wa_lfa1_pc-lifnr IS INITIAL.
      CLEAR: wa_lfa1_pc,
             wa_trolz.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CALC_DURACAO_TRANSITO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_calc_duracao_transito .

  IF v_distancia IS NOT INITIAL.
    v_dur_tran = v_distancia / 24.
    v_dur_tran = v_dur_tran / 60.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SALVAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_salvar_dados .

  DATA: lt_tvro      TYPE TABLE OF tvro,
        lt_tvro_t    TYPE TABLE OF tvrot,
        lt_troiz     TYPE TABLE OF troiz,
        lt_trolz     TYPE TABLE OF trolz,
        lv_duracao   TYPE char10,
        lv_msg       TYPE string,
        lv_mode      TYPE c VALUE 'N',
        lt_messages  TYPE TABLE OF bdcmsgcoll,
        li_qtd       TYPE numc2,
        lv_field     TYPE bdcdata-fnam,
        lv_distancia TYPE string,
        lv_route     TYPE route,
        lv_dist_km   TYPE distz,               "*-CS2024000954-10.12.2024-#155195-JT
        lt_zlest0232 TYPE TABLE OF zlest0232,
        lt_zlest0027 TYPE TABLE OF zlest0027.  "*-CS2024000877-06.12.2024-#154505-JT

*-CS2024000954-10.12.2024-#155195-JT-inicio
  SELECT SINGLE low
    INTO @DATA(_km_max)
    FROM tvarvc
   WHERE name = 'KM_MAXIMO'.

  IF sy-subrc = 0.
    lv_dist_km = _km_max.

    IF v_distancia > lv_dist_km.
      MESSAGE s024(sd) WITH 'Distancia informada maior que limite permitido! '
                            'Duvidas entre em contato com o depto de logística.' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.
*-CS2024000954-10.12.2024-#155195-JT-fim

  IF wa_kna1-kunnr IS INITIAL OR wa_lfa1_pc-lifnr IS INITIAL OR
     v_distancia IS INITIAL.
    MESSAGE 'Favor preencher campos obrigatórios!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF wa_kna1-lzone IS INITIAL OR wa_lfa1_pc-lzone IS INITIAL.
    MESSAGE 'Dados inválidos, favor corrigir' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT *
    FROM trolz
    INTO @DATA(ls_trolz)
    UP TO 1 ROWS
    WHERE azone = @wa_lfa1_pc-lzone
      AND lzone = @wa_kna1-lzone.
  ENDSELECT.
  IF sy-subrc IS INITIAL.
    MESSAGE 'Já existe itinerário para as Zonas de Saída e Destino informado' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  APPEND INITIAL LINE TO lt_tvro ASSIGNING FIELD-SYMBOL(<fs_tvro>).
  APPEND INITIAL LINE TO lt_tvro_t ASSIGNING FIELD-SYMBOL(<fs_tvro_t>).

  CONCATENATE wa_lfa1_pc-name1(18) 'X' wa_kna1-name1(18) INTO <fs_tvro_t>-bezei SEPARATED BY space.
  <fs_tvro_t>-spras = sy-langu.

  CONCATENATE wa_lfa1_pc-name1 'X' wa_kna1-name1 INTO <fs_tvro>-routid SEPARATED BY space.

  lv_duracao = v_dur_tran.
  REPLACE '.' IN lv_duracao WITH ','.

  CALL FUNCTION 'CONVERSION_EXIT_TSTRG_INPUT'
    EXPORTING
      input          = lv_duracao
    IMPORTING
      output         = <fs_tvro>-traztd
    EXCEPTIONS
      invalid_format = 1
      OTHERS         = 2.

  FREE: gt_bdcdata.

  lv_distancia = v_distancia.
  CONDENSE lv_distancia NO-GAPS.
  REPLACE FIRST OCCURRENCE OF '.' IN lv_distancia WITH ','.

  PERFORM f_bdcdata USING 'SAPL0VTR' '2000' 'X' '' ''.
  PERFORM f_bdcdata USING '' '' '' 'BDC_OKCODE' '=NEWL'.
  PERFORM f_bdcdata USING 'SAPL0VTR' '2010' 'X' '' ''.
  PERFORM f_bdcdata USING '' '' '' 'BDC_OKCODE' '/00'.
  PERFORM f_bdcdata USING '' '' '' 'V_TVRO_COM-BEZEI' <fs_tvro_t>-bezei.
  PERFORM f_bdcdata USING '' '' '' 'V_TVRO_COM-ROUTID' <fs_tvro>-routid.
  PERFORM f_bdcdata USING '' '' '' 'V_TVRO_COM-VSART' v_tp_exped.
  PERFORM f_bdcdata USING '' '' '' 'V_TVRO_COM-MEDST' v_km.
  PERFORM f_bdcdata USING '' '' '' 'V_TVRO_COM-TDIIX' v_chk_relev.
  PERFORM f_bdcdata USING '' '' '' 'V_TVRO_COM-DISTZ' lv_distancia.
  PERFORM f_bdcdata USING '' '' '' 'V_TVRO_COM-SPFBK' v_cal_fab.
  PERFORM f_bdcdata USING '' '' '' 'V_TVRO_COM-TRAZTD' lv_duracao.
  PERFORM f_bdcdata USING 'SAPL0VTR' '2010' 'X' '' ''.
  PERFORM f_bdcdata USING '' '' '' 'BDC_OKCODE' '=SAVE'.
  PERFORM f_bdcdata USING 'SAPL0VTR' '2010' 'X' '' ''.
  PERFORM f_bdcdata USING '' '' '' 'BDC_OKCODE' '=UEBE'.
  PERFORM f_bdcdata USING 'SAPL0VTR' '2000' 'X' '' ''.
  PERFORM f_bdcdata USING '' '' '' 'BDC_OKCODE' '=BACK'.

  CALL TRANSACTION '0VTC' USING gt_bdcdata MODE lv_mode MESSAGES INTO lt_messages.

  FREE: gt_bdcdata.

  IMPORT lv_route TO lv_route FROM MEMORY ID 'ZLESR0162_ROUTE_NUMBER'.

  IF lv_route IS NOT INITIAL.

    FREE MEMORY ID 'ZLESR0162_ROUTE_NUMBER'.

* RJF - Ini - 2023.06.07 - CS2022000329 AUTOMATIZAÇÃO DA CRIAÇÃO DE ITINERÁRIOS - Ajuste de mapeamento SHDB #114506
    PERFORM f_bdcdata USING 'SAPLSVIX' '0210' 'X' '' ''.
    PERFORM f_bdcdata USING '' '' '' 'BDC_CURSOR' 'MARK_CHECKBOX(04)'.
    PERFORM f_bdcdata USING '' '' '' 'BDC_OKCODE' '=OKAY'.
    PERFORM f_bdcdata USING '' '' '' 'MARK_CHECKBOX(02)' 'X'.
    PERFORM f_bdcdata USING '' '' '' 'MARK_CHECKBOX(04)' 'X'.

    PERFORM f_bdcdata USING 'SAPLSVIX' '0100' 'X' '' ''.
    PERFORM f_bdcdata USING '' '' '' 'BDC_CURSOR' 'D0100_FIELD_TAB-UPPER_LIMIT(02)'.
    PERFORM f_bdcdata USING '' '' '' 'BDC_OKCODE' '=OKAY'.
    PERFORM f_bdcdata USING '' '' '' 'D0100_FIELD_TAB-LOWER_LIMIT(01)' wa_lfa1_pc-lzone.
    PERFORM f_bdcdata USING '' '' '' 'D0100_FIELD_TAB-LOWER_LIMIT(02)' wa_kna1-lzone.
    PERFORM f_bdcdata USING '' '' '' 'D0100_FIELD_TAB-UPPER_LIMIT(01)' wa_lfa1_pc-lzone.
    PERFORM f_bdcdata USING '' '' '' 'D0100_FIELD_TAB-UPPER_LIMIT(02)' wa_kna1-lzone.
* RJF - Fim - 2023.06.07 - CS2022000329 AUTOMATIZAÇÃO DA CRIAÇÃO DE ITINERÁRIOS - Ajuste de mapeamento SHDB #114506

    PERFORM f_bdcdata USING 'SAPL0VRF' '2000' 'X' '' ''.
    PERFORM f_bdcdata USING '' '' '' 'BDC_OKCODE' '=NEWL'.
    PERFORM f_bdcdata USING 'SAPL0VRF' '2000' 'X' '' ''.
    PERFORM f_bdcdata USING '' '' '' 'BDC_OKCODE' '=SAVE'.
    PERFORM f_bdcdata USING '' '' '' 'V_TROLZ00-ALAND(01)' 'BR'.
    PERFORM f_bdcdata USING '' '' '' 'V_TROLZ00-AZONE(01)'  wa_lfa1_pc-lzone.
    PERFORM f_bdcdata USING '' '' '' 'V_TROLZ00-LLAND(01)' 'BR'.
    PERFORM f_bdcdata USING '' '' '' 'V_TROLZ00-LZONE(01)' wa_kna1-lzone.
    PERFORM f_bdcdata USING 'SAPL0VRF' '2000' 'X' '' ''.
    PERFORM f_bdcdata USING '' '' '' 'BDC_OKCODE' '=DET2'.
    PERFORM f_bdcdata USING '' '' '' 'VIM_MARKED(01)' 'X'.
    PERFORM f_bdcdata USING 'SAPL0VRF' '2100' 'X' '' ''.
    PERFORM f_bdcdata USING '' '' '' 'BDC_OKCODE' '=NEWL'.
    PERFORM f_bdcdata USING 'SAPL0VRF' '2100' 'X' '' ''.
    PERFORM f_bdcdata USING '' '' '' 'BDC_OKCODE' '=SAVE'.

    SELECT *
      FROM ttgr
      INTO TABLE @DATA(lt_ttgr).
    IF sy-subrc IS INITIAL.

      LOOP AT lt_ttgr ASSIGNING FIELD-SYMBOL(<fs_ttgr>).
        ADD 1 TO li_qtd.

        CONCATENATE 'V_TROLZ01-VSBED(' li_qtd ')' INTO lv_field.

        PERFORM f_bdcdata USING '' '' '' lv_field '01'.

        CONCATENATE 'V_TROLZ01-TRAGR(' li_qtd ')' INTO lv_field.

        PERFORM f_bdcdata USING '' '' '' lv_field <fs_ttgr>-tragr.

        CONCATENATE 'V_TROLZ01-ROUTE(' li_qtd ')' INTO lv_field.

        PERFORM f_bdcdata USING '' '' '' lv_field lv_route.

      ENDLOOP.

    ENDIF.

    CLEAR li_qtd.

    PERFORM f_bdcdata USING 'SAPL0VRF' '2100' 'X' '' ''.
    PERFORM f_bdcdata USING '' '' '' 'BDC_OKCODE' '=DET3'.
    PERFORM f_bdcdata USING 'SAPL0VRF' '2200' 'X' '' ''.
    PERFORM f_bdcdata USING '' '' '' 'BDC_OKCODE' '=NEWL'.
    PERFORM f_bdcdata USING 'SAPL0VRF' '2200' 'X' '' ''.
    PERFORM f_bdcdata USING '' '' '' 'BDC_OKCODE' '=SAVE'.
    PERFORM f_bdcdata USING '' '' '' 'V_TROLZ02-ROUTE(01)' lv_route.
    PERFORM f_bdcdata USING '' '' '' 'V_TROLZ02-VSBED(01)' '01'.
    PERFORM f_bdcdata USING '' '' '' 'V_TROLZ02-TRAGR(01)' '0001'.
    PERFORM f_bdcdata USING '' '' '' 'V_TROLZ02-GRULG(01)' '0001'.
    PERFORM f_bdcdata USING 'SAPL0VRF' '2200' 'X' '' ''.
    PERFORM f_bdcdata USING '' '' '' 'BDC_OKCODE' '=BACK'.
    PERFORM f_bdcdata USING 'SAPL0VRF' '2200' 'X' '' ''.
    PERFORM f_bdcdata USING '' '' '' 'BDC_OKCODE' '=BACK'.
    PERFORM f_bdcdata USING 'SAPL0VRF' '2000' 'X' '' ''.
    PERFORM f_bdcdata USING '' '' '' 'BDC_OKCODE' '=BACK'.
    PERFORM f_bdcdata USING 'SAPL0VRF' '2000' 'X' '' ''.
    PERFORM f_bdcdata USING '' '' '' 'BDC_OKCODE' '=BACK'.

*    CALL TRANSACTION '0VRF' USING gt_bdcdata MODE lv_mode MESSAGES INTO lt_messages.
    CALL TRANSACTION 'Z0VRF' USING gt_bdcdata MODE lv_mode MESSAGES INTO lt_messages. "RJF

    FREE: gt_bdcdata.

    WAIT UP TO 8 SECONDS.

    PERFORM f_busca_itinerario.

    CONCATENATE 'Itinerário' wa_trolz-route 'gerado com sucesso' INTO lv_msg SEPARATED BY space.
    MESSAGE lv_msg TYPE 'S'.

    EXPORT lv_route FROM wa_trolz-route TO MEMORY ID 'ZLESR0162_ROUTE'.

  ENDIF.

  IF wa_trolz-route IS NOT INITIAL.

    APPEND INITIAL LINE TO lt_zlest0232 ASSIGNING FIELD-SYMBOL(<fs_zlest0232>).

    <fs_zlest0232>-route      = wa_trolz-route.
    <fs_zlest0232>-routbez    = wa_trolz-desc.
    <fs_zlest0232>-routid     = <fs_tvro>-routid.
    <fs_zlest0232>-versart    = 'BR'.
    <fs_zlest0232>-distz      = v_distancia.
    <fs_zlest0232>-medst      = 'KM'.
    <fs_zlest0232>-tdiix      = 'X'.
    <fs_zlest0232>-traztd     = <fs_tvro>-traztd.
    <fs_zlest0232>-fabkl      = 'BR'.
    <fs_zlest0232>-ck_pedagio = v_ck_pedagio.  "*-CS2024000877-06.12.2024-#154505-JT
    <fs_zlest0232>-ernam      = sy-uname.
    <fs_zlest0232>-erdat      = sy-datum.
    <fs_zlest0232>-erzet      = sy-uzeit.

    MODIFY zlest0232 FROM TABLE lt_zlest0232.

*-CS2024000877-06.12.2024-#154505-JT-inicio
    APPEND INITIAL LINE TO lt_zlest0027 ASSIGNING FIELD-SYMBOL(<fs_zlest0027>).
    <fs_zlest0027>-route      = wa_trolz-route.
    <fs_zlest0027>-ck_pedagio = v_ck_pedagio.
    MODIFY zlest0027 FROM TABLE lt_zlest0027.
*-CS2024000877-06.12.2024-#154505-JT-fim

    IF sy-subrc IS INITIAL.
      COMMIT WORK.
    ENDIF.

  ELSE.

    PERFORM f_bdcdata USING 'SAPL0VTR' '2000' 'X' '' ''.
    PERFORM f_bdcdata USING '' '' '' 'BDC_OKCODE' '=POSI'.
    PERFORM f_bdcdata USING 'SAPLSPO4' '0300' 'X' '' ''.
    PERFORM f_bdcdata USING '' '' '' 'BDC_OKCODE' '=FURT'.
    PERFORM f_bdcdata USING '' '' '' 'SVALD-VALUE(01)' lv_route.
    PERFORM f_bdcdata USING 'SAPL0VTR' '2000' 'X' '' ''.
    PERFORM f_bdcdata USING '' '' '' 'BDC_OKCODE' '=DELE'.
    PERFORM f_bdcdata USING '' '' '' 'VIM_MARKED(01)' 'X'.
    PERFORM f_bdcdata USING 'SAPL0VTR' '2000' 'X' '' ''.
    PERFORM f_bdcdata USING '' '' '' 'BDC_OKCODE' '=SAVE'.
    PERFORM f_bdcdata USING 'SAPL0VTR' '2000' 'X' '' ''.
    PERFORM f_bdcdata USING '' '' '' 'BDC_OKCODE' '=BACK'.

    CALL TRANSACTION '0VTC' USING gt_bdcdata MODE lv_mode MESSAGES INTO lt_messages.

    FREE: gt_bdcdata.

  ENDIF.

  SET PARAMETER ID 'COD_CLI' FIELD space.
  SET PARAMETER ID 'COD_PC'  FIELD space.
  SET PARAMETER ID 'ZONA_DES'  FIELD space.
  SET PARAMETER ID 'ZONA_ORI'  FIELD space.

  CLEAR: v_distancia,
         v_dur_tran,
         wa_kna1,
         wa_lfa1_pc,
         wa_trolz,
         v_kunnr,
         v_lifnr.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALTERA_ZONA_LR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM altera_zona_lr .

  DATA: ls_selfield        TYPE slis_selfield.

  DATA(vl_kunnr) = |{ wa_kna1-kunnr ALPHA = IN }|.

  SELECT * FROM zlest0153   INTO TABLE @DATA(it_zlest0153)     WHERE land1 NE ' ' AND kunnr = @vl_kunnr.

  "FF #143875 -Ini
  SELECT * FROM kna1                                        "FF #143875
  APPENDING CORRESPONDING FIELDS OF TABLE @it_zlest0153[]
  WHERE land1 NE ' ' AND kunnr = @vl_kunnr.

*  SELECT * FROM kna1        INTO TABLE @DATA(tl_kna1)   WHERE kunnr = @vl_kunnr.
*  IF ( sy-subrc = 0 ).
*    it_zlest0153[] = CORRESPONDING #( tl_kna1[] ).
*  ENDIF.
  "FF #143875 -Fim


  CHECK ( it_zlest0153[] IS NOT INITIAL ).
  SORT it_zlest0153[] BY lzone ASCENDING.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_title              = 'Selecione uma Zona de Transporte'
      i_selection          = 'X'
      i_zebra              = 'X'
      i_allow_no_selection = 'X'
      i_tabname            = 1
      i_structure_name     = 'ZLEST0153'
    IMPORTING
      es_selfield          = ls_selfield
    TABLES
      t_outtab             = it_zlest0153[]
    EXCEPTIONS
      program_error        = 1
      OTHERS               = 2.

  CHECK ( ls_selfield IS NOT INITIAL ).

  wa_kna1-lzone = it_zlest0153[ ls_selfield-tabindex ]-lzone.
  lv_zone_des = wa_kna1-lzone.
**<<<------"187200 - NMS - INI------>>>
  SELECT SINGLE zlatitude zlongitude FROM tzone
    INTO ( wa_kna1-zlatitude, wa_kna1-zlongitude )
  WHERE land1 EQ wa_kna1-land1 AND zone1 EQ wa_kna1-lzone.
**<<<------"187200 - NMS - FIM------>>>
  SELECT SINGLE vtext FROM tzont INTO wa_kna1-zone_desc
    WHERE spras EQ 'P' AND land1 EQ 'BR' AND zone1 EQ wa_kna1-lzone.

  "AO ALTERAR A ZONA, REFAZER A BUSCA DE ITINERÁRIO.
  IF ( wa_lfa1_pc-lzone IS NOT INITIAL ) AND ( wa_kna1-lzone IS NOT INITIAL ).
    PERFORM f_busca_itinerario.
  ENDIF.

*  vg_focus = 'WA_KNA1-ZONE_DESC'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALTERA_ZONA_PC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM altera_zona_pc .

  DATA: ls_selfield TYPE slis_selfield,
        it_z0153    TYPE TABLE OF zlese0153,
        wa_z0153    TYPE zlese0153.

  DATA(vl_lifnr) = |{ wa_lfa1_pc-lifnr ALPHA = IN }|.

  REFRESH it_z0153.

  SELECT * FROM zlest0153 INTO TABLE @DATA(it_zlest0153)
    WHERE land1 NE ' ' AND lifnr = @vl_lifnr.

  SELECT * FROM lfa1 APPENDING CORRESPONDING FIELDS OF TABLE it_zlest0153[]
    WHERE lifnr = vl_lifnr.

  SORT it_zlest0153[] BY lzone ASCENDING.

  LOOP AT it_zlest0153 INTO DATA(wa_zlest0153).

    SELECT SINGLE *
      FROM tzont INTO @DATA(wa_tzont)
     WHERE zone1 EQ @wa_zlest0153-lzone
      AND  spras EQ 'P'
      AND land1  EQ 'BR'.

    IF sy-subrc = 0.

      wa_z0153-land1        = wa_zlest0153-land1.
      wa_z0153-vtext        = wa_tzont-vtext.
      wa_z0153-lzone        = wa_zlest0153-lzone.
      wa_z0153-lifnr        = wa_zlest0153-lifnr.
      wa_z0153-kunnr        = wa_zlest0153-kunnr.
      wa_z0153-us_registro  = wa_zlest0153-us_registro.
      wa_z0153-dt_registro  = wa_zlest0153-dt_registro.
      wa_z0153-hr_registro  = wa_zlest0153-hr_registro.

      APPEND wa_z0153 TO it_z0153.
      CLEAR :  wa_z0153, wa_zlest0153.
    ENDIF.
  ENDLOOP.


  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_title              = 'Selecione uma Zona de Transporte'
      i_selection          = 'X'
      i_zebra              = 'X'
      i_tabname            = 1
      i_structure_name     = 'ZLESE0153'
      i_allow_no_selection = 'X'
    IMPORTING
      es_selfield          = ls_selfield
    TABLES
      t_outtab             = it_z0153[]
    EXCEPTIONS
      program_error        = 1
      OTHERS               = 2.

  IF ( ls_selfield IS NOT INITIAL ).

    wa_lfa1_pc-lzone = it_zlest0153[ ls_selfield-tabindex ]-lzone.
    lv_zone_ori = wa_lfa1_pc-lzone.
**<<<------"187200 - NMS - INI------>>>
    SELECT SINGLE zlatitude zlongitude FROM tzone
      INTO ( wa_lfa1_pc-zlatitude, wa_lfa1_pc-zlongitude )
    WHERE land1 EQ wa_lfa1_pc-land1 AND zone1 EQ wa_lfa1_pc-lzone.
**<<<------"187200 - NMS - FIM------>>>
    SELECT SINGLE vtext FROM tzont INTO wa_lfa1_pc-zone_desc
        WHERE spras = 'P' AND land1 = 'BR' AND zone1 = wa_lfa1_pc-lzone.

  ENDIF.

  IF ( wa_lfa1_pc-lzone IS NOT INITIAL ) AND ( wa_kna1-lzone IS NOT INITIAL ).
    PERFORM f_busca_itinerario.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*     FORM BUSCA_ITINERARIO .
*---------------------------------------------------------------------*
FORM f_busca_itinerario .

  DATA: lv_answer TYPE c,
        lv_route  TYPE tvro-route.

  CHECK ( wa_lfa1_pc-lzone IS NOT INITIAL AND wa_kna1-lzone IS NOT INITIAL ).

  CLEAR wa_trolz-route.

  SELECT SINGLE aland, azone, lland, lzone, route
    FROM trolz INTO @DATA(w_trolz)
    WHERE aland EQ 'BR'
      AND azone EQ @wa_lfa1_pc-lzone
      AND lland EQ 'BR'
      AND lzone EQ @wa_kna1-lzone.

  IF ( sy-subrc = 0 ).

    wa_trolz-route = w_trolz-route.

*-CS2024000877-06.12.2024-#154505-JT-inicio
    SELECT SINGLE ck_pedagio
      INTO @DATA(_ck_pedagio)
      FROM zlest0027
     WHERE route = @w_trolz-route.

    IF sy-subrc = 0.
      LOOP AT SCREEN.
        IF screen-name = 'V_CK_PEDAGIO'.
          screen-input = '0'.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

      v_ck_pedagio = _ck_pedagio.
    ENDIF.
*-CS2024000877-06.12.2024-#154505-JT-fim

    SELECT SINGLE bezei FROM tvrot INTO wa_trolz-desc
      WHERE spras = 'P' AND route = wa_trolz-route.

  ELSE.

    CLEAR: wa_trolz-desc.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BDCDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0948   text
*      -->P_0949   text
*      -->P_0950   text
*      -->P_0951   text
*      -->P_0952   text
*----------------------------------------------------------------------*
FORM f_bdcdata  USING    VALUE(p_program)
                         VALUE(p_screen)
                         VALUE(p_new)
                         VALUE(p_field)
                         VALUE(p_value).

  APPEND INITIAL LINE TO gt_bdcdata ASSIGNING FIELD-SYMBOL(<fs_bdcdata>).

  <fs_bdcdata>-program  = p_program.
  <fs_bdcdata>-dynpro   = p_screen.
  <fs_bdcdata>-dynbegin = p_new.
  <fs_bdcdata>-fnam     = p_field.
  <fs_bdcdata>-fval     = p_value.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EXECUTE_JOB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_execute_job .

  DATA: lv_kunnr  TYPE kna1-kunnr,
        lv_lifnr  TYPE lfa1-lifnr,
        lt_fields TYPE TABLE OF rsdcf,
        lt_lines  TYPE TABLE OF tline.

  lv_zone_des  = p_lzone.
  lv_zone_ori  = p_azone.
  lv_kunnr     = p_kunnr.
  lv_lifnr     = p_lifnr.
  v_distancia  = p_dist.
  v_ck_pedagio = p_ck_ped.  "*-CS2024000877-06.12.2024-#154505-JT

  IF lv_kunnr IS NOT INITIAL.
    wa_kna1-kunnr = lv_kunnr.
  ENDIF.

  IF lv_lifnr IS NOT INITIAL.
    wa_lfa1_pc-lifnr = lv_lifnr.
  ENDIF.

  PERFORM f_preenche_campos.
  PERFORM f_salvar_dados.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_JOB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_create_job .

  DATA: lv_jobcount  TYPE btcjobcnt,
        lv_start     TYPE btch0000-char1 VALUE 'X',
        lv_perc      TYPE sy-tabix,
        lt_zlest0232 TYPE TABLE OF zlest0232,
        lr_job       TYPE REF TO zcl_job,
        lv_qtd       TYPE sy-tabix,
        lv_server    TYPE btctgtsrvr-srvname,
        lv_msg       TYPE string,
        lt_sellist   TYPE TABLE OF vimsellist,
        lv_time      TYPE sy-tabix,
        lv_dist_km   TYPE distz,               "*-CS2024000954-10.12.2024-#155195-JT
        lv_esgotado  TYPE c,
        lt_status    TYPE TABLE OF c.

*-CS2024000954-10.12.2024-#155195-JT-inicio
  SELECT SINGLE low
    INTO @DATA(_km_max)
    FROM tvarvc
   WHERE name = 'KM_MAXIMO'.

  IF sy-subrc = 0.
    lv_dist_km = _km_max.

    IF v_distancia > lv_dist_km.
      MESSAGE s024(sd) WITH 'Distancia informada maior que limite permitido! '
                            'Duvidas entre em contato com o depto de logística.' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.
*-CS2024000954-10.12.2024-#155195-JT-fim

  IF wa_kna1-kunnr IS INITIAL OR wa_lfa1_pc-lifnr IS INITIAL OR
     v_distancia IS INITIAL OR wa_kna1-lzone IS INITIAL OR wa_lfa1_pc-lzone IS INITIAL
    OR p_pedagio IS INITIAL. "170657 CS2025000299 CHECKBOX TO LIST ZLES0214 PSA
    MESSAGE 'Favor preecher os campos obrigatórios' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF wa_trolz-route IS NOT INITIAL.
    MESSAGE 'Já existe itinerário para as Zonas de Saída e Destino informado' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.


  IF p_pedagio IS NOT INITIAL."170657 CS2025000299 CHECKBOX TO LIST ZLES0214 PSA

    CASE p_pedagio.
      WHEN 'SIM'.
        v_ck_pedagio = abap_true.
      WHEN 'NÃO'.
        v_ck_pedagio = abap_false.
      WHEN OTHERS.
    ENDCASE.

  ENDIF.

  CREATE OBJECT lr_job.

  SELECT *
    FROM tvarvc
    INTO TABLE @DATA(lt_tvarv)
    WHERE ( name = 'ZJOB_DELETE'
       OR   name = 'ZJOB_SERVIDOR'
       OR   name = 'ZJOB_TIME' ) .
  IF sy-subrc IS INITIAL.
    SORT lt_tvarv BY name.

    READ TABLE lt_tvarv ASSIGNING FIELD-SYMBOL(<fs_tvarv>)
    WITH KEY name = 'ZJOB_SERVIDOR'
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      lv_server = <fs_tvarv>-low.
    ENDIF.

    READ TABLE lt_tvarv ASSIGNING <fs_tvarv>
    WITH KEY name = 'ZJOB_TIME'
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      lv_time = <fs_tvarv>-low.
    ELSE.
      lv_time = 45.
    ENDIF.

  ENDIF.

  APPEND INITIAL LINE TO lt_status ASSIGNING FIELD-SYMBOL(<fs_status>).
  <fs_status> = 'R'.

  DO lv_time TIMES.

    lv_perc = 10.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = lv_perc
        text       = 'Processando Criação de Itinerário. Aguarde!'.

    TRY.
        CALL METHOD zcl_job=>get_job_programa_execucao
          EXPORTING
            i_job_name   = 'ZLESR0162_JOB'
            i_status     = lt_status
          IMPORTING
            e_quantidade = lv_qtd.

        IF lv_qtd IS INITIAL OR lv_qtd EQ 0.
          EXIT.
        ENDIF.
      CATCH zcx_job .
        EXIT.
    ENDTRY.

    IF sy-index = lv_time.
      lv_esgotado = abap_true.
      EXIT.
    ELSE.
      WAIT UP TO 2 SECONDS.
    ENDIF.

  ENDDO.

  IF lv_esgotado IS NOT INITIAL.

    CLEAR lv_esgotado.

    CALL FUNCTION 'VIEW_ENQUEUE'
      EXPORTING
        view_name        = 'V_TVKN_COM'
        action           = 'E'
        enqueue_mode     = 'S'
        enqueue_range    = 'X'
      TABLES
        sellist          = lt_sellist
      EXCEPTIONS
        foreign_lock     = 1
        system_failure   = 2
        table_not_found  = 5
        client_reference = 7.
    IF sy-subrc EQ '1'.

      CONCATENATE 'Os dados estão bloqueados pelo usuario' sy-msgv1  INTO lv_msg SEPARATED BY space.

      MESSAGE lv_msg TYPE 'I'.

    ELSE.

      CALL FUNCTION 'VIEW_ENQUEUE'
        EXPORTING
          view_name        = 'V_TVKN_COM'
          action           = 'D'
          enqueue_mode     = 'S'
          enqueue_range    = 'X'
        TABLES
          sellist          = lt_sellist
        EXCEPTIONS
          foreign_lock     = 1
          system_failure   = 2
          table_not_found  = 5
          client_reference = 7.

      MESSAGE 'Tempo excedido, tente novamente V_TVKN_COM ' TYPE 'S' DISPLAY LIKE 'E'.

      CALL FUNCTION 'VIEW_ENQUEUE'
        EXPORTING
          view_name        = 'V_TROLZ02'
          action           = 'E'
          enqueue_mode     = 'S'
          enqueue_range    = 'X'
        TABLES
          sellist          = lt_sellist
        EXCEPTIONS
          foreign_lock     = 1
          system_failure   = 2
          table_not_found  = 5
          client_reference = 7.
      IF sy-subrc EQ '1'.

        CONCATENATE 'Os dados estão bloqueados pelo usuario' sy-msgv1  INTO lv_msg SEPARATED BY space.

        MESSAGE lv_msg TYPE 'I'.

      ELSE.

        CALL FUNCTION 'VIEW_ENQUEUE'
          EXPORTING
            view_name        = 'V_TROLZ02'
            action           = 'D'
            enqueue_mode     = 'S'
            enqueue_range    = 'X'
          TABLES
            sellist          = lt_sellist
          EXCEPTIONS
            foreign_lock     = 1
            system_failure   = 2
            table_not_found  = 5
            client_reference = 7.

        MESSAGE 'Tempo excedido, tente novamente V_TROLZ02' TYPE 'S' DISPLAY LIKE 'E'.

      ENDIF.

    ENDIF.

    EXIT.

  ENDIF.

*-CS2024000877-06.12.2024-#154505-JT-inicio
  IF 1 = 2.
    SUBMIT zlesr0162 AND RETURN WITH p_kunnr  = wa_kna1-kunnr
                                WITH p_lifnr  = wa_lfa1_pc-lifnr
                                WITH p_azone  = wa_lfa1_pc-lzone
                                WITH p_lzone  = wa_kna1-lzone
                                WITH p_dist   = v_distancia
                                WITH p_ck_ped = v_ck_pedagio.   "*-CS2024000877-06.12.2024-#154505-JT
  ENDIF.
*-CS2024000877-06.12.2024-#154505-JT-fim

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = 'ZLESR0162_JOB'
    IMPORTING
      jobcount         = lv_jobcount
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.
  IF sy-subrc IS INITIAL.

    SET PARAMETER ID 'ZONA_DES' FIELD wa_kna1-lzone.
    SET PARAMETER ID 'ZONA_ORI' FIELD wa_lfa1_pc-lzone.
    SET PARAMETER ID 'COD_CLI' FIELD wa_kna1-kunnr.
    SET PARAMETER ID 'COD_PC' FIELD wa_lfa1_pc-lifnr.

    SUBMIT zlesr0162 AND RETURN WITH p_kunnr  = wa_kna1-kunnr
                                WITH p_lifnr  = wa_lfa1_pc-lifnr
                                WITH p_azone  = wa_lfa1_pc-lzone
                                WITH p_lzone  = wa_kna1-lzone
                                WITH p_dist   = v_distancia
                                WITH p_ck_ped = v_ck_pedagio   "*-CS2024000877-06.12.2024-#154505-JT
                                USER 'JOBADM'
                                VIA JOB 'ZLESR0162_JOB' NUMBER lv_jobcount.

  ENDIF.

  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount             = lv_jobcount
      jobname              = 'ZLESR0162_JOB'
      strtimmed            = lv_start
      targetserver         = lv_server
    EXCEPTIONS
      cant_start_immediate = 1
      invalid_startdate    = 2
      jobname_missing      = 3
      job_close_failed     = 4
      job_nosteps          = 5
      job_notex            = 6
      lock_failed          = 7
      invalid_target       = 8
      OTHERS               = 9.
  IF sy-subrc = 0.

    DO lv_time TIMES.

      lv_perc = 70.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = lv_perc
          text       = 'Processando Criação de Itinerário. Aguarde!'.

      SELECT SINGLE aland, azone, lland, lzone, route
          FROM trolz INTO @DATA(w_trolz)
          WHERE aland EQ 'BR'
            AND azone EQ @wa_lfa1_pc-lzone
            AND lland EQ 'BR'
            AND lzone EQ @wa_kna1-lzone.
      IF sy-subrc IS NOT INITIAL.

        WAIT UP TO 2 SECONDS.

      ELSE.

        wa_trolz-route = w_trolz-route.
        PERFORM f_busca_itinerario.

        CONCATENATE 'Itinerário' wa_trolz-route 'gerado com sucesso' INTO lv_msg SEPARATED BY space.
        MESSAGE lv_msg TYPE 'S'.

        READ TABLE lt_tvarv ASSIGNING <fs_tvarv>
        WITH KEY name = 'ZJOB_DELETE'
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          IF <fs_tvarv>-low EQ 'S'.

            CALL FUNCTION 'BP_JOB_DELETE'
              EXPORTING
                jobcount                 = lv_jobcount
                jobname                  = 'ZLESR0162_JOB'
              EXCEPTIONS
                cant_delete_event_entry  = 1
                cant_delete_job          = 2
                cant_delete_joblog       = 3
                cant_delete_steps        = 4
                cant_delete_time_entry   = 5
                cant_derelease_successor = 6
                cant_enq_predecessor     = 7
                cant_enq_successor       = 8
                cant_enq_tbtco_entry     = 9
                cant_update_predecessor  = 10
                cant_update_successor    = 11
                commit_failed            = 12
                jobcount_missing         = 13
                jobname_missing          = 14
                job_does_not_exist       = 15
                job_is_already_running   = 16
                no_delete_authority      = 17
                OTHERS                   = 18.
            IF sy-subrc = 0.

            ENDIF.

          ENDIF.

        ENDIF.

        EXIT.

      ENDIF.

    ENDDO.

  ENDIF.

  IF wa_trolz-route IS INITIAL.

    MESSAGE 'Tempo excedido, tente novamente ZLESR0162_JOB' TYPE 'S' DISPLAY LIKE 'E'.

  ELSE.

    EXPORT lv_route FROM wa_trolz-route TO MEMORY ID 'ZLESR0162_ROUTE'.

    SET PARAMETER ID 'COD_CLI' FIELD space.
    SET PARAMETER ID 'COD_PC'  FIELD space.
    SET PARAMETER ID 'ZONA_DES'  FIELD space.
    SET PARAMETER ID 'ZONA_ORI'  FIELD space.

    CLEAR: v_distancia,
           v_dur_tran,
           wa_kna1,
           wa_lfa1_pc,
           wa_trolz,
           v_kunnr,
           v_lifnr,
           lv_zone_des,
           lv_zone_ori,
           p_pedagio."170657 CS2025000299 CHECKBOX TO LIST ZLES0214 PSA

  ENDIF.

  CLEAR: sy-ucomm.

ENDFORM.
