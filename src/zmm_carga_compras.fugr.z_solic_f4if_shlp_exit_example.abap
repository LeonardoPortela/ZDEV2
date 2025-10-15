FUNCTION z_solic_f4if_shlp_exit_example.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------

* EXIT immediately, if you do not want to handle this step
  IF callcontrol-step <> 'SELONE' AND
     callcontrol-step <> 'SELECT' AND
     " AND SO ON
     callcontrol-step <> 'DISP'.
    EXIT.
  ENDIF.

*"----------------------------------------------------------------------
* STEP SELONE  (Select one of the elementary searchhelps)
*"----------------------------------------------------------------------
* This step is only called for collective searchhelps. It may be used
* to reduce the amount of elementary searchhelps given in SHLP_TAB.
* The compound searchhelp is given in SHLP.
* If you do not change CALLCONTROL-STEP, the next step is the
* dialog, to select one of the elementary searchhelps.
* If you want to skip this dialog, you have to return the selected
* elementary searchhelp in SHLP and to change CALLCONTROL-STEP to
* either to 'PRESEL' or to 'SELECT'.
  IF callcontrol-step = 'SELONE'.
*   PERFORM SELONE .........
    EXIT.
  ENDIF.

*"----------------------------------------------------------------------
* STEP PRESEL  (Enter selection conditions)
*"----------------------------------------------------------------------
* This step allows you, to influence the selection conditions either
* before they are displayed or in order to skip the dialog completely.
* If you want to skip the dialog, you should change CALLCONTROL-STEP
* to 'SELECT'.
* Normaly only SHLP-SELOPT should be changed in this step.
  IF callcontrol-step = 'PRESEL'.
*   PERFORM PRESEL ..........
    EXIT.
  ENDIF.
*"----------------------------------------------------------------------
* STEP SELECT    (Select values)
*"----------------------------------------------------------------------
* This step may be used to overtake the data selection completely.
* To skip the standard seletion, you should return 'DISP' as following
* step in CALLCONTROL-STEP.
* Normally RECORD_TAB should be filled after this step.
* Standard function module F4UT_RESULTS_MAP may be very helpfull in this
* step.
  IF callcontrol-step = 'SELECT'.
*   PERFORM STEP_SELECT TABLES RECORD_TAB SHLP_TAB
*                       CHANGING SHLP CALLCONTROL RC.
*   IF RC = 0.
*     CALLCONTROL-STEP = 'DISP'.
*   ELSE.
*     CALLCONTROL-STEP = 'EXIT'.
*   ENDIF.
    EXIT. "Don't process STEP DISP additionally in this call.
  ENDIF.
*"----------------------------------------------------------------------
* STEP DISP     (Display values)
*"----------------------------------------------------------------------
* This step is called, before the selected data is displayed.
* You can e.g. modify or reduce the data in RECORD_TAB
* according to the users authority.
* If you want to get the standard display dialog afterwards, you
* should not change CALLCONTROL-STEP.
* If you want to overtake the dialog on you own, you must return
* the following values in CALLCONTROL-STEP:
* - "RETURN" if one line was selected. The selected line must be
*   the only record left in RECORD_TAB. The corresponding fields of
*   this line are entered into the screen.
* - "EXIT" if the values request should be aborted
* - "PRESEL" if you want to return to the selection dialog
* Standard function modules F4UT_PARAMETER_VALUE_GET and
* F4UT_PARAMETER_RESULTS_PUT may be very helpfull in this step.
  IF callcontrol-step = 'DISP'.
*   PERFORM AUTHORITY_CHECK TABLES RECORD_TAB SHLP_TAB
*                           CHANGING SHLP CALLCONTROL.

    TYPES: BEGIN OF ty_0202,
             nro_sol TYPE zmmt0202-nro_sol,
             seq     TYPE char3,
             total   TYPE zmmt0202-qtd_vinc_carga,
           END OF ty_0202.

    DATA: lt_0196_aux TYPE TABLE OF zmmt0196,
          lt_0202_sum TYPE TABLE OF ty_0202,
          lw_0202_sum TYPE ty_0202,
          lv_string   TYPE string,
          lv_mkl      TYPE mara-matkl.

    GET PARAMETER ID 'MKL' FIELD lv_mkl.

    LOOP AT record_tab ASSIGNING FIELD-SYMBOL(<fs_record>).

      lv_string = <fs_record>-string.
      CONDENSE lv_string NO-GAPS.

      APPEND INITIAL LINE TO lt_0196_aux ASSIGNING FIELD-SYMBOL(<fs_0196_aux>).

      <fs_0196_aux>-nro_sol = lv_string(10).
      <fs_0196_aux>-seq = lv_string+10(3).

    ENDLOOP.

    IF lt_0196_aux IS NOT INITIAL.

      SELECT *
        FROM zmmt0196
        INTO TABLE @DATA(lt_0196)
        FOR ALL ENTRIES IN @lt_0196_aux
        WHERE nro_sol = @lt_0196_aux-nro_sol
          AND seq     = @lt_0196_aux-seq
          AND solicitacao_qte > 0
          AND cancel NE @abap_true.
      IF sy-subrc IS INITIAL.
        SORT lt_0196 BY nro_sol seq.
      ENDIF.

    ENDIF.

    IF lt_0196 IS NOT INITIAL.

      SELECT *
        FROM ekpo
        INTO TABLE @DATA(lt_ekpo)
        FOR ALL ENTRIES IN @lt_0196
        WHERE ebeln = @lt_0196-ebeln
          AND ebelp = @lt_0196-ebelp
          AND matkl = @lv_mkl.
      IF sy-subrc IS INITIAL.
        SORT lt_ekpo BY ebeln ebelp.
      ENDIF.

      SELECT *
        FROM zmmt0202
        INTO TABLE @DATA(lt_0202)
        FOR ALL ENTRIES IN @lt_0196
        WHERE nro_sol = @lt_0196-nro_sol
          AND seq     = @lt_0196-seq.
      IF sy-subrc IS INITIAL.
        SORT lt_0202 BY nro_sol seq.
        LOOP AT lt_0202 ASSIGNING FIELD-SYMBOL(<fs_0202>).
          lw_0202_sum-nro_sol = <fs_0202>-nro_sol.
          lw_0202_sum-seq     = <fs_0202>-seq.
          lw_0202_sum-total   = <fs_0202>-qtd_vinc_carga.

          COLLECT lw_0202_sum INTO lt_0202_sum.
        ENDLOOP.
      ENDIF.

    ENDIF.
    SORT lt_0202_sum BY nro_sol seq.

    LOOP AT lt_0196_aux ASSIGNING <fs_0196_aux>.

      READ TABLE lt_0196 ASSIGNING FIELD-SYMBOL(<fs_0196>)
      WITH KEY nro_sol = <fs_0196_aux>-nro_sol
               seq     = <fs_0196_aux>-seq
      BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL.
        lv_string = <fs_0196_aux>-nro_sol && <fs_0196_aux>-seq.
        DELETE record_tab WHERE string CS lv_string.
        CONTINUE.
      ELSE.
        READ TABLE lt_ekpo TRANSPORTING NO FIELDS
        WITH KEY ebeln = <fs_0196>-ebeln
                 ebelp = <fs_0196>-ebelp
        BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          lv_string = <fs_0196_aux>-nro_sol && <fs_0196_aux>-seq.
          DELETE record_tab WHERE string CS lv_string.
          CONTINUE.
        ENDIF.
      ENDIF.

      READ TABLE lt_0202_sum  ASSIGNING FIELD-SYMBOL(<fs_0202_sum>)
      WITH KEY nro_sol = <fs_0196_aux>-nro_sol
               seq     = <fs_0196_aux>-seq
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        IF <fs_0202_sum>-total >= <fs_0196>-solicitacao_qte.
          lv_string = <fs_0196>-nro_sol && <fs_0196>-seq.
          DELETE record_tab WHERE string CS lv_string.
        ENDIF.
      ENDIF.
    ENDLOOP.

    EXIT.
  ENDIF.
ENDFUNCTION.
