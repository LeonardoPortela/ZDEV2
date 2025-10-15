*&---------------------------------------------------------------------*
*&  Include           ZPPR0028_FORM
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Form  ZAPONTAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zapontamento.
**  Begin of    #96115  FF  07.03.2023
  CLEAR vg_erro.
  DATA(lv_aufnr) = |{ v_ordem ALPHA = IN }|.

  SELECT SINGLE werks FROM caufv INTO zaponta-werks
    WHERE aufnr = lv_aufnr.

  IF sy-subrc = 0 AND zaponta-werks IS NOT INITIAL.
    READ TABLE it_zpmt0012 WITH KEY iwerk = zaponta-werks TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      vg_erro = 'X'.
      MESSAGE i000(o0) WITH 'Empregado' w_ztpm_d_usuario-pernr 'não cadastrado para o centro de trabalho' zaponta-werks .
    ENDIF.
  ENDIF.
** End of FF  07.03.2023

**  Begin of    #96115  FF  07.03.2023
  READ TABLE it_zpmt0012 WITH KEY transacao = c_apontamento
                                  criar = 'X'
                                  iwerk = zaponta-werks TRANSPORTING NO FIELDS.
  IF sy-subrc <> 0.
    vg_erro = 'X'.
    MESSAGE i000 WITH 'Código do empregado'
                       w_ztpm_d_usuario-pernr
                      'sem permissão para fazer apontamento no centro'
                      zaponta-werks.
  ENDIF.

  CHECK vg_erro IS INITIAL.
** End of FF  07.03.2023

  IF obj_main->t_aufk IS NOT INITIAL .
    CLEAR zaponta.


    zaponta-aufnr       = v_ordem.
    zaponta-activity    = v_vornr.
*    zaponta-description = v_ktext.
**  Begin of    #96115  FF
    READ TABLE lt_operations WITH KEY activity = v_vornr INTO DATA(wa_op).
    IF sy-subrc = 0.
      zaponta-description = wa_op-description.
    ENDIF.

    zaponta-pernr       = w_ztpm_d_usuario-pernr.
    zaponta-sname       = w_ztpm_d_usuario-cname.
    zaponta-isdd  = zaponta-budat =
    zaponta-iedd  = sy-datum.
** End of FF

*    READ TABLE lt_operations WITH KEY activity = v_vornr INTO DATA(wa_operations).
*    IF sy-subrc = 0.
*      zaponta-duration_normal_unit = wa_operations-duration_normal_unit.
*    ELSE.
*      CLEAR zaponta-duration_normal_unit.
*    ENDIF.

*    IF zaponta-afrud >= 1.
*      zaponta-duration_normal_unit = 'H'.
*    ELSE.
*      zaponta-duration_normal_unit = 'MIN'.
*    ENDIF.
**  Begin of   #96115  FF
*    CALL SCREEN 0200 STARTING AT 5 5 ENDING AT 86 24. "Tela desativada
    CALL SCREEN 0201 STARTING AT 5 5 ENDING AT 125 15.
* * End of FF
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  GET CURSOR FIELD w_cursor_field.

  IF sy-ucomm = '' AND w_cursor_field = 'ZAPONTA-IEDZ'.
    sy-ucomm = 'V_CONFIRMA'.
  ENDIF.

  CASE sy-ucomm.
    WHEN 'VOLTAR'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'CLEAR'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'V_CANCELAR'.
      PERFORM clear_tela.
      LEAVE TO SCREEN 0.
    WHEN 'V_CONFIRMA'.
      CLEAR vg_erro.
      PERFORM check_operacao CHANGING vg_erro."ins "FF #96115
      IF vg_erro IS INITIAL.
        PERFORM validar_dados.
        PERFORM limpar_campos. "Hora, Ação e Trab. Real
      ENDIF.
    WHEN 'ENTER'.
      PERFORM check_operacao CHANGING vg_erro.
    WHEN 'V_NOVO'.
      PERFORM clear_tela.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  SELEC_OPERACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selec_operacao .
  FREE it_aufk[].
  SELECT a~aufnr a~vaplz b~aufpl c~vornr c~ltxa1 a~werks e~ktext
  FROM aufk AS a
  INNER JOIN afko AS b ON b~aufnr = a~aufnr
  INNER JOIN afvc AS c ON c~aufpl = b~aufpl
  INNER JOIN crhd AS d ON d~arbpl = a~vaplz AND d~werks = a~werks
  INNER JOIN crtx AS e ON e~objid = d~objid
  INTO CORRESPONDING FIELDS OF TABLE it_aufk
  WHERE a~aufnr EQ zaponta-aufnr
    AND c~vornr EQ zaponta-activity.


  IF it_aufk[] IS INITIAL .
    MESSAGE i000(o0) WITH 'A operação' zaponta-activity 'não existe na ordem' zaponta-aufnr DISPLAY LIKE 'E'.
  ELSE.
    READ TABLE it_aufk WITH KEY aufnr = zaponta-aufnr
                                vornr = zaponta-activity.

    IF sy-subrc = 0.
      zaponta-description          = it_aufk-ltxa1.
      zaponta-werks                = it_aufk-werks.

      IF zaponta-duration_normal_unit IS INITIAL.
        zaponta-duration_normal_unit = 'H'.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELEC_EMPREGADO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selec_empregado .
  DATA wa_empregado TYPE ty_hrp1001.
  DATA: cont_ctrab TYPE p.
  DATA: hinic TYPE sy-uzeit.
  DATA: hfim  TYPE sy-uzeit.
  DATA: hpaus TYPE char8.

  IF zaponta-pernr IS NOT INITIAL.
    FREE it_empregado[].
    SELECT DISTINCT a~objid b~kapid c~sobid d~pernr d~sname a~arbpl a~werks f~ktext
    FROM crhd AS a
    INNER JOIN crca    AS b ON b~objid = a~objid
    INNER JOIN hrp1001 AS c ON c~objid = b~kapid
    INNER JOIN pa0001  AS d ON d~pernr = c~sobid
    INNER JOIN aufk    AS e ON e~werks = a~werks
    INNER JOIN crtx    AS f ON f~objid = a~objid
    INTO CORRESPONDING FIELDS OF TABLE it_empregado
    WHERE c~sobid EQ zaponta-pernr
      AND e~aufnr EQ zaponta-aufnr
      AND a~werks EQ zaponta-werks
     AND  c~otype EQ 'KA'.
    SORT it_empregado ASCENDING BY pernr.
    DELETE ADJACENT DUPLICATES FROM it_empregado COMPARING pernr objid.

    IF it_empregado[] IS NOT INITIAL.
      CLEAR cont_ctrab.
      LOOP AT it_empregado.
        ADD 1 TO cont_ctrab.
      ENDLOOP.

      READ TABLE it_empregado WITH KEY  pernr = zaponta-pernr
                                        werks = zaponta-werks.

      IF zaponta-sname <> it_empregado-sname.
        IF cont_ctrab > 1.
          CALL SCREEN 0300 STARTING AT 5 5 ENDING AT 70 20. "Tela de selação centro de trabalho.
        ELSE.
          READ TABLE it_empregado WITH KEY  pernr = zaponta-pernr
                                            werks = zaponta-werks.
          IF sy-subrc = 0.
            zaponta-sname     = it_empregado-sname.
            zaponta-work_cntr = it_empregado-arbpl.
            zaponta-ktext     = it_empregado-ktext.
          ENDIF.
        ENDIF.
      ENDIF.

**  Begin of   "FF #96115  aqui
      IF zaponta-sname IS INITIAL OR
         zaponta-work_cntr IS INITIAL.

        zaponta-sname     = it_empregado-sname.
        zaponta-work_cntr = it_empregado-arbpl.
        zaponta-ktext     = it_empregado-ktext.
      ENDIF.
** End of FF

    ELSE.
*      MESSAGE TEXT-003 TYPE 'I' DISPLAY LIKE 'E'.
      MESSAGE i000(o0) WITH 'Código do empregado' zaponta-pernr 'não cadastrado para um centro de trabalho'.
      CLEAR zaponta-sname.
      CLEAR it_empregado-sname.
      CLEAR it_empregado-ktext.
      CLEAR zaponta-ktext.
      CLEAR it_empregado-arbpl.
      CLEAR zaponta-work_cntr.
      CLEAR zaponta-begzt.
      CLEAR zaponta-endzt.
      CLEAR zaponta-pause.
      CLEAR zaponta-einzt.
      CLEAR zaponta-ngrad.
      CLEAR zaponta-ueberlast.
      CLEAR zaponta-v_sobrcarg.
      CLEAR zaponta-einzh.
    ENDIF.
  ENDIF.

  FREE it_kako[].
  IF zaponta-work_cntr IS NOT INITIAL.
    SELECT c~kapid a~werks c~begzt c~endzt c~pause c~ueberlast c~ngrad
  FROM crhd AS a
  INNER JOIN crca AS b ON b~objid = a~objid
  INNER JOIN kako AS c ON c~kapid = b~kapid
  INTO CORRESPONDING FIELDS OF TABLE it_kako
  WHERE a~arbpl EQ zaponta-work_cntr
    AND a~werks EQ zaponta-werks.

    IF it_kako[] IS NOT INITIAL.
      LOOP AT it_kako.

        zaponta-begzt     =  it_kako-begzt.
        zaponta-endzt     =  it_kako-endzt.
        zaponta-pause     =  it_kako-pause.

        it_kako-begzt = ( it_kako-begzt / 60 ) / 60.
        it_kako-endzt = ( it_kako-endzt / 60 ) / 60.
        it_kako-pause = ( it_kako-pause / 60 ) / 60.

        zaponta-einzt     = ( it_kako-endzt - it_kako-begzt ) - it_kako-pause.
        zaponta-ngrad     = ( it_kako-ngrad / 100 ) * 100.
        zaponta-ueberlast = ( it_kako-ueberlast / 100 ) * 100.

        zaponta-einzt      = ( zaponta-einzt * zaponta-ngrad ) / 100.
        zaponta-v_sobrcarg = ( zaponta-einzt * zaponta-ueberlast ) / 100.
        zaponta-einzh      = ( zaponta-einzt + zaponta-v_sobrcarg ).
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELEC_DESC_PARADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selec_desc_parada .

  IF zaponta-grund IS NOT INITIAL.
    SELECT *
    FROM trugt
    INTO CORRESPONDING FIELDS OF TABLE it_selec_parada
    WHERE grund EQ zaponta-grund
      AND werks EQ zaponta-werks
      AND spras EQ 'P'.

    IF it_selec_parada[] IS NOT INITIAL.
      LOOP AT it_selec_parada.
        CLEAR zaponta-grdtx.
        zaponta-grdtx = it_selec_parada-grdtx.
      ENDLOOP.
    ELSE.
      MESSAGE i000(o0) WITH 'Causa desvio' zaponta-grund 'não existe para o centro' zaponta-werks DISPLAY LIKE 'E'.
      CLEAR it_selec_parada-grdtx.
      CLEAR zaponta-grdtx.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CLEAR_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_tela .
  CLEAR: zaponta-description,
         zaponta-work_cntr,
         zaponta-pernr,
         zaponta-sname,
         zaponta-grund,
         zaponta-grdtx,
         zaponta-afrud,
         zaponta-duration_normal_unit,
         zaponta-isdd ,
         zaponta-isdz ,
         zaponta-iedd ,
         zaponta-iedz ,
         zaponta-begzt,
         zaponta-endzt,
         zaponta-pause,
         zaponta-einzt,
         zaponta-ngrad,
         zaponta-ueberlast,
         zaponta-v_sobrcarg,
         zaponta-einzh,
         zaponta-ktext,
         zaponta-fin_conf,
         zaponta-duration_normal_unit,
         zaponta-mncod,
         zaponta-txtcdma,
         soma_total.
*         ZAPONTA-ACTIVITY.
*
*  IF V_ORDEM IS NOT INITIAL.
*    CLEAR ZAPONTA-AUFNR.
*    ZAPONTA-AUFNR = V_ORDEM.
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_OPERACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_operacao CHANGING p_erro.
  FREE it_aufk[].
  IF zaponta-activity IS INITIAL.
    p_erro = 'X'.
    MESSAGE i000(o0) WITH 'É necessário atribuir a ordem' zaponta-aufnr 'a um nº de operação'.
  ELSE.
    IF zaponta-activity IS NOT INITIAL.
      PERFORM selec_operacao.
    ENDIF.

    IF zaponta-pernr IS NOT INITIAL.
      IF it_aufk-ltxa1 IS NOT INITIAL.
        PERFORM selec_empregado.
      ELSE.
        IF sy-uname <> 'ABAP'.
          p_erro = 'X'.
          MESSAGE i000(o0) WITH 'A operação' zaponta-activity 'não existe na ordem' zaponta-aufnr DISPLAY LIKE 'E'.
        ENDIF.
      ENDIF.
    ELSE.
      CLEAR zaponta-sname.
      CLEAR it_empregado-ktext.
      CLEAR zaponta-ktext.
      CLEAR it_empregado-arbpl.
      CLEAR zaponta-work_cntr.
      CLEAR zaponta-begzt.
      CLEAR zaponta-endzt.
      CLEAR zaponta-pause.
      CLEAR zaponta-einzt.
      CLEAR zaponta-ngrad.
      CLEAR zaponta-ueberlast.
      CLEAR zaponta-v_sobrcarg.
      CLEAR zaponta-duration_normal_unit.
      CLEAR zaponta-einzh.
    ENDIF.

    IF zaponta-isdd IS NOT INITIAL.
      IF it_empregado-sname IS NOT INITIAL.

      ELSE.
*        MESSAGE TEXT-003 TYPE 'I' DISPLAY LIKE 'E'.
*        MESSAGE I000(O0) WITH 'Código do empregado' ZAPONTA-PERNR 'não cadastrado para o centro de trabalho' ZAPONTA-WORK_CNTR DISPLAY LIKE 'E'.
        CLEAR zaponta-sname.
        CLEAR it_empregado-ktext.
        CLEAR zaponta-ktext.
        CLEAR it_empregado-arbpl.
        CLEAR zaponta-work_cntr.
        CLEAR zaponta-begzt.
        CLEAR zaponta-endzt.
        CLEAR zaponta-pause.
        CLEAR zaponta-einzt.
        CLEAR zaponta-ngrad.
        CLEAR zaponta-ueberlast.
        CLEAR zaponta-v_sobrcarg.
*        CLEAR zaponta-duration_normal_unit. "#96115  FF
        CLEAR zaponta-einzh.
      ENDIF.
    ENDIF.

    IF zaponta-grund IS NOT INITIAL.
      PERFORM selec_desc_parada.
    ENDIF.

*    IF zaponta-isdd IS NOT INITIAL.
*      zaponta-budat = zaponta-isdd.
*    ELSE.
*      CLEAR zaponta-budat.
*    ENDIF.

    CASE abap_false.
      WHEN zaponta-isdd OR zaponta-isdz OR zaponta-iedd OR zaponta-iedz.
        EXIT.
    ENDCASE.

    IF zaponta-isdd IS NOT INITIAL AND zaponta-iedd IS NOT INITIAL.

      IF zaponta-isdd > zaponta-iedd.
        p_erro = 'X'.
        MESSAGE 'Data limite ultrapassada para lançamento.' TYPE 'I' DISPLAY LIKE 'E'.
      ELSE.
        IF  zaponta-isdd IS NOT INITIAL
          AND zaponta-isdz IS NOT INITIAL
            AND zaponta-iedd IS NOT INITIAL
              AND zaponta-iedz IS NOT INITIAL.

          IF zaponta-isdd > sy-datum OR zaponta-iedd > sy-datum OR zaponta-isdd = sy-datum AND zaponta-isdz > sy-uzeit OR zaponta-iedd = sy-datum AND zaponta-iedz > sy-uzeit.
            p_erro = 'X'.
            MESSAGE i209(ru) DISPLAY LIKE 'E'.
          ELSE.
            IF zaponta-isdd = zaponta-iedd
               AND zaponta-isdz > zaponta-iedz.
              p_erro = 'X'.
              MESSAGE 'Data limite ultrapassada para lançamento.' TYPE 'I' DISPLAY LIKE 'E'.
            ELSE.

*              DATA(ld_beg_da) = zaponta-isdd.
*              DATA(ld_end_da) = zaponta-iedd.

              CLEAR: ld_no_day, ld_no_month, ld_no_year, ld_no_cal_day, soma_data, soma_hora, soma_total.

*              "Calculando total de dias.
*              CALL FUNCTION 'HR_AUPBS_MONTH_DAY'
*                EXPORTING
*                  beg_da     = ld_beg_da
*                  end_da     = ld_end_da
*                IMPORTING
*                  no_day     = ld_no_day
*                  no_month   = ld_no_month
*                  no_year    = ld_no_year
*                  no_cal_day = ld_no_cal_day.
*
*              "Calculando total de horas.
*              soma_data     = ( ld_no_cal_day - 1 ) * 24.
*              soma_hora     = ( zaponta-iedz  - zaponta-isdz  ) / 60.
*              soma_hora     = ( soma_hora / 60 ).
*              soma_total    = ( soma_data + soma_hora ).

**  Begin of    #96115  FF
**  Begin 09/07/2025  PM - IR246191 - 2000048058 - Erro na transação ZPM0102 e IW47
** Comentar rutina actual
              DATA: lv_time_difference TYPE i,
                    lv_hours_int       TYPE i,
                    lv_hours           TYPE p DECIMALS 2,
                    lv_minutes         TYPE p DECIMALS 2.

*              lv_time_difference = ( zaponta-iedd - zaponta-isdd ) * 24 * 3600 +
*                                   ( zaponta-iedz - zaponta-isdz ).
*
*              lv_hours = lv_time_difference / 3600.
*              lv_hours_int = trunc( lv_hours ). "Pegando a parte inteira do decimal
*              lv_minutes = ( lv_time_difference MOD 3600 ) / 60.
*
*              soma_total = lv_hours_int + lv_minutes / 100.
*
*              zaponta-afrud = soma_total.
*              IF zaponta-afrud >= 1.
*                zaponta-duration_normal_unit = 'H'.
*              ELSE.
*                zaponta-duration_normal_unit = 'MIN'.
*                zaponta-afrud = zaponta-afrud * 100.
*              ENDIF.
** End of FF
              DATA : lv_timediff  TYPE fahztd.
              DATA : lv_time(10).
              DATA : lv_mn(2)     TYPE n.
              DATA : lv_hr(4)     TYPE n.

              CALL FUNCTION 'SD_CALC_DURATION_FROM_DATETIME'
                EXPORTING
                  i_date1 = zaponta-isdd
                  i_time1 = zaponta-isdz
                  i_date2 = zaponta-iedd
                  i_time2 = zaponta-iedz
                IMPORTING
                  e_tdiff = lv_timediff.

              IF NOT lv_timediff IS INITIAL.
                CALL FUNCTION 'CONVERSION_EXIT_TSTRN_OUTPUT'
                  EXPORTING
                    input  = lv_timediff
                  IMPORTING
                    output = lv_time.

                REPLACE ALL OCCURRENCES OF '.' IN lv_time WITH space.
                CONDENSE lv_time NO-GAPS.
                SPLIT  lv_time AT ':' INTO lv_hr lv_mn.
                IF NOT lv_hr > 0. " Minutos.
                  soma_total    = lv_mn / 100.
                  zaponta-afrud = soma_total.
                  zaponta-duration_normal_unit = 'MIN'.
                ELSE.
                  zaponta-duration_normal_unit = 'H'.
                  IF NOT lv_mn IS INITIAL.
                    IF lv_mn BETWEEN 1 AND 30.
                      lv_mn = 30.
                    ELSE.
                      IF lv_mn  BETWEEN 31 AND 59.
                        lv_mn = 60.
                      ENDIF.
                    ENDIF.
                    soma_total  = lv_hr + ( lv_mn / 60 ).
                  ELSE.
                    soma_total = lv_hr.
                  ENDIF.
                  zaponta-afrud = soma_total.
                ENDIF.

*
*              IF zaponta-afrud >= 1.
*                zaponta-duration_normal_unit = 'H'.
*              ELSE.
*                zaponta-duration_normal_unit = 'MIN'.
*              ENDIF.
              ENDIF.
**  Begin 09/07/2025  PM - IR246191 - 2000048058 - Erro na transação ZPM0102 e IW47
              IF soma_total > 0.
                IF soma_total > zaponta-einzh."IT_KAKO-V_EINZH.
                  p_erro = 'X'.
                  MESSAGE i000(o0) WITH 'Carga horaria informada' soma_total 'é maior que tempo de utilização cadastrado' zaponta-einzh DISPLAY LIKE 'E'.
                ENDIF.
              ELSE.
                p_erro = 'X'.
                MESSAGE 'Data limite ultrapassada para lançamento.' TYPE 'I' DISPLAY LIKE 'E'.
                EXIT.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.


        "Check inicio parada.
        CLEAR: ws_viqmel.
        SELECT SINGLE * FROM viqmel INTO ws_viqmel
          WHERE aufnr EQ zaponta-aufnr.
        IF sy-subrc EQ 0.
          IF ws_viqmel-msaus EQ abap_true.
            CLEAR: vg_dt_hr_init_parada.
            vg_dt_hr_init_parada = |{ ws_viqmel-ausvn }{ ws_viqmel-auztv }|.
            vg_dt_hr_init_apont  = |{ zaponta-isdd }{ zaponta-isdz }|.

            IF vg_dt_hr_init_apont < vg_dt_hr_init_parada.
              MESSAGE e024(sd) WITH 'Inicio do apontamento é menor que inicio da parada'
                                    'Informada na abertura da nota->' ws_viqmel-qmnum.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELEC_INF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selec_inf .

  CLEAR vg_erro.

  DATA: zretun                TYPE p DECIMALS 2,
        v_ponta               TYPE p DECIMALS 2,
        cont_erro             TYPE p DECIMALS 2,
        wa_apont              TYPE ty_zaponta,
        lv_user_format_dt(10) TYPE c.
  .

  IF zaponta IS NOT INITIAL.
    FREE t_afru[].
    FREE tl_afru[].
*    Check se ja existe lançamento para empregado no mesmo periodo para mesma ordem ou ordens diferente.
    SELECT *
    FROM afru
    INTO CORRESPONDING FIELDS OF TABLE t_afru
    WHERE pernr EQ zaponta-pernr
      AND stokz NE abap_true
      AND stzhl EQ ' '.

    SORT t_afru ASCENDING BY isdd isdz.
    CLEAR zretun.
    CLEAR v_ponta.

**  Begin of   "FF #96115
    "Somando o ismnw, porém se o ismne = H, divido por 60 pra fazer a soma em minutos.

    SELECT SUM(
        CASE WHEN ismne = 'H' THEN ismnw * 60
             WHEN ismne = 'MIN' THEN ismnw
             ELSE 0
        END
    )
    INTO @DATA(lv_total_minutos)
    FROM afru
    WHERE pernr = @zaponta-pernr
        AND isdd = @zaponta-isdd
        AND stokz <> 'X'
        AND stzhl = @space.

    IF sy-subrc = 0 AND lv_total_minutos IS NOT INITIAL.

      DATA(lv_total_hr_apontadas) = lv_total_minutos / 60.

      IF sy-subrc = 0 AND lv_total_hr_apontadas >= '10'.
        cont_erro = 1.

        lv_user_format_dt = |{ zaponta-isdd DATE = USER }|. "Transforma a data no formato definido pelo usuário.

        DATA(lv_msg) = |{ 'No dia' }| &
                       | { lv_user_format_dt }| &
                       | { 'já foram apontadas' }| &
                       | { lv_total_hr_apontadas }| &
                       | { 'horas.' }|.

        MESSAGE i000 WITH  lv_msg DISPLAY LIKE 'E'.
      ENDIF.
    ENDIF.
** End of FF

    LOOP AT t_afru WHERE pernr = zaponta-pernr
                    AND   isdd = zaponta-isdd OR iedd = zaponta-iedd.

      IF sy-subrc = 0.
        IF zaponta-pernr = t_afru-pernr AND zaponta-isdd BETWEEN t_afru-isdd AND t_afru-iedd.
* Ini - IR184083 - 2000009700 - Stefanini - PRB - Inclusão de Validação de Hora Fim.
* Inclusão de validação sobre a hora final informada.
*         IF zaponta-pernr = t_afru-pernr AND zaponta-isdz BETWEEN t_afru-isdz AND t_afru-iedz.
          IF zaponta-pernr = t_afru-pernr AND zaponta-isdz BETWEEN t_afru-isdz AND t_afru-iedz
                                           OR zaponta-iedz BETWEEN t_afru-isdz AND t_afru-iedz.
* Ini - IR184083 - 2000009700 - Stefanini - PRB - Inclusão de Validação de Hora Fim.

            ADD 1 TO zretun.

            MOVE-CORRESPONDING t_afru TO tl_afru.
            APPEND tl_afru.
          ENDIF.
        ENDIF.
      ENDIF.
      CLEAR t_afru.
    ENDLOOP.


    LOOP AT it_aponta INTO wa_apont WHERE pernr = zaponta-pernr
                                     AND   isdd = zaponta-isdd OR iedd = zaponta-iedd.

      IF zaponta-pernr = wa_apont-pernr AND zaponta-isdd BETWEEN wa_apont-isdd AND wa_apont-iedd.
        IF zaponta-pernr = wa_apont-pernr AND zaponta-isdz BETWEEN wa_apont-isdz AND wa_apont-iedz.
          ADD 1 TO zretun.
        ENDIF.

      ENDIF.
      CLEAR wa_apont.
    ENDLOOP.

    LOOP AT it_aponta ASSIGNING FIELD-SYMBOL(<l_apont>) WHERE aufnr = zaponta-aufnr
                                                       AND activity = zaponta-activity.
      IF <l_apont>-fin_conf IS NOT INITIAL.
        MESSAGE i000 WITH 'Confimarção final ja foi selecionada'DISPLAY LIKE 'E'.
        ADD 1 TO cont_erro.
        CONTINUE.
      ENDIF.
    ENDLOOP.


    IF zretun = 0.
*      Check calculo total de horas digitado.
      LOOP AT t_afru  ASSIGNING FIELD-SYMBOL(<w_afru>) WHERE pernr = zaponta-pernr
                                                          AND isdd = zaponta-isdd.


        IF <w_afru>-isdd = zaponta-isdd AND <w_afru>-pernr IS NOT INITIAL.
          IF <w_afru>-ismne = 'MIN'.
            <w_afru>-ismnw = ( <w_afru>-ismnw / 60 ).
            ADD <w_afru>-ismnw TO v_ponta.
          ELSE.

            ADD <w_afru>-ismnw TO v_ponta.
          ENDIF.
        ENDIF.
      ENDLOOP.

      DATA lv_afrud TYPE zoperations-afrud.
      LOOP AT it_aponta ASSIGNING FIELD-SYMBOL(<w_apont>) WHERE pernr = zaponta-pernr
                                                            AND isdd  = zaponta-isdd.

        IF <w_apont>-isdd  = zaponta-isdd AND <w_apont>-pernr IS NOT INITIAL.
          IF <w_apont>-duration_normal_unit = 'MIN'.
*            <w_apont>-afrud = ( <w_apont>-afrud / 60 ).
            lv_afrud = <w_apont>-afrud / 100.
            ADD  lv_afrud TO v_ponta.

          ELSE.
            ADD <w_apont>-afrud TO v_ponta.
          ENDIF.
        ENDIF.
      ENDLOOP.

*      IF ZAPONTA-DURATION_NORMAL_UNIT = 'MIN'.
*        ZAPONTA-AFRUD = ( ZAPONTA-AFRUD / 60 ).
*      ENDIF.

*      v_ponta = ( v_ponta + zaponta-afrud ).
      v_ponta = ( v_ponta + soma_total ).

      IF v_ponta > zaponta-einzh.
        ADD 1 TO cont_erro.
        MESSAGE i008 WITH v_ponta zaponta-einzh DISPLAY LIKE 'E'.
      ELSE.
        IF cont_erro IS INITIAL.
          APPEND zaponta TO it_aponta.
          MOVE-CORRESPONDING it_aponta TO obj_main->it_opera.
          SORT it_aponta ASCENDING BY pernr isdd isdz.
          SORT obj_main->it_opera ASCENDING BY pernr isdd isdz.
          CLEAR cont_erro.
        ELSE.
          CLEAR cont_erro.
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE i009 WITH zaponta-pernr zaponta-sname DISPLAY LIKE 'E'.
      IF tl_afru[] IS NOT INITIAL.
        CALL SCREEN 0400 STARTING AT 5 5 ENDING AT 110 20.
      ENDIF.
    ENDIF.
  ENDIF.

  IF cont_erro > 0.
    vg_erro = 'X'.
  ELSE.
    CLEAR vg_erro.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VALIDAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validar_dados .

**  Begin of   "FF #96115
*    DATA(fieldname) = 'ZAPONTA-ISDD'.  "Nome do campo onde você deseja definir o cursor
*    SET CURSOR FIELD fieldname .
*
*    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
*      EXPORTING
*        functioncode           = '=ENTER' "ENTER
*      EXCEPTIONS
*        function_not_supported = 1
*        OTHERS                 = 2.

** End of FF

  IF "zaponta-work_cntr              IS INITIAL "#96115  FF

     zaponta-pernr  IS INITIAL
  OR zaponta-isdd   IS INITIAL
  OR zaponta-isdz   IS INITIAL
  OR zaponta-iedd   IS INITIAL
  OR zaponta-iedz   IS INITIAL
*  OR zaponta-afrud  IS INITIAL
  OR zaponta-budat  IS INITIAL
  OR zaponta-mncod  IS INITIAL.
    MESSAGE TEXT-005 TYPE 'I' DISPLAY LIKE 'E'. "i025(bbp_at) DISPLAY LIKE 'E'. ".
  ELSE.
    PERFORM selec_inf.
    IF vg_erro IS INITIAL.                                  "#96115  FF
      IF zaponta-fin_conf IS NOT INITIAL.                   "#96115  FF
        PERFORM clear_tela.
        LEAVE TO SCREEN 0.                                  "#96115  FF
      ENDIF.
      obj_main->alv1->refresh_table_display( is_stable = obj_main->stable )."STABLE
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  BUSCA_EMPREGADO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_empregado INPUT.

  SELECT a~werks a~arbpl c~sobid d~pernr d~sname a~objid b~kapid f~ktext
  FROM crhd AS a
  INNER JOIN crca    AS b ON b~objid = a~objid
  INNER JOIN hrp1001 AS c ON c~objid = b~kapid
  INNER JOIN pa0001  AS d ON d~pernr = c~sobid
  INNER JOIN aufk    AS e ON e~werks = a~werks
  INNER JOIN crtx    AS f ON f~objid = a~objid
    INTO CORRESPONDING FIELDS OF TABLE it_hrp1001
    WHERE e~aufnr EQ zaponta-aufnr
      AND c~otype EQ 'KA'.
  SORT it_hrp1001 ASCENDING BY pernr.
  DELETE ADJACENT DUPLICATES FROM it_hrp1001 COMPARING pernr objid.


  CHECK it_hrp1001[] IS NOT INITIAL.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'PERNR'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'PERNR'
      value_org       = 'S'
    TABLES
      value_tab       = it_hrp1001
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  BUSCA_INTERVALO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_intervalo INPUT.
  DATA: t_trugt TYPE TABLE OF trugt.

**  Begin of    #96115  FF
  DATA: t_mapping TYPE STANDARD TABLE OF dselc,

        p_cod     TYPE help_info-dynprofld VALUE 'ZAPONTA-GRUND',
        p_desc    TYPE help_info-dynprofld VALUE 'ZAPONTA-GRDTX'.

*====>  Work Area
  DATA: s_return  TYPE ddshretval.
  DATA: s_mapping TYPE dselc.
** End of FF

  SELECT SINGLE werks FROM caufv
    INTO zaponta-werks
  WHERE aufnr = zaponta-aufnr.

  IF sy-subrc <> 0.
    CLEAR zaponta-werks.
  ENDIF.

  FREE t_trugt.
  SELECT *
  FROM trugt
  INTO TABLE t_trugt
  WHERE werks EQ zaponta-werks
   AND  spras EQ 'PT'.

  CHECK t_trugt IS NOT INITIAL.

**  Begin of    #96115  FF
  s_mapping-fldname     = 'F0004'.
  s_mapping-dyfldname   = p_cod.
  APPEND s_mapping TO t_mapping.
  CLEAR s_mapping.

  s_mapping-fldname     = 'F0005'.
  s_mapping-dyfldname   = p_desc.
  APPEND s_mapping TO t_mapping.
  CLEAR s_mapping.
** End of FF


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'GRUND'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = p_cod
      value_org       = 'S'
    TABLES
      value_tab       = t_trugt
      return_tab      = tl_return_tab
      dynpfld_mapping = t_mapping.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.

  DATA:
 lst_layout TYPE lvc_s_layo.

  DATA: url(255)                TYPE c,
        p_text                  TYPE sdydo_text_element,
        p_text_2                TYPE sdydo_text_element,
        sdydo_text_element(255),
        p_text_table            TYPE sdydo_text_table,
        p_text_table_2          TYPE sdydo_text_table,
        vl_cont                 TYPE i,
        vl_butxt                TYPE t001-butxt,
        vl_dates1               TYPE char10,
        vl_dates2               TYPE char10.

  SET PF-STATUS 'T0300'.
  SET TITLEBAR 'T0301'.

*  FREE G_CUSTOM_CONTAINER.
* Adicionando Logo Marca no Cabeçalho
  IF g_custom_container IS INITIAL.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

*    IF  IT_EMPREGADO[] IS NOT INITIAL.

    PERFORM fill_it_fieldcatalog USING:
          1  'WERKS'  'IT_EMPREGADO '  '20'   ' '  ' '  ' '  'Centro            '  ''  ''  'CRHD     '  ' ',
          2  'ARBPL'  'IT_EMPREGADO '  '20'   ' '  ' '  ' '  'C.trabalho        '  ''  ''  'CRHD     '  ' ',
          3  'PERNR'  'IT_EMPREGADO '  '20'   ' '  ' '  ' '  'Cod empregado     '  ''  ''  'PA0001   '  ' ',
          4  'SNAME'  'IT_EMPREGADO '  '20'   ' '  ' '  ' '  'Nome              '  ''  ''  'PA0001   '  ' '.


    gs_layout-sel_mode   = 'A'.
    gs_layout-cwidth_opt = 'X'.
    CLEAR: it_exclude_fcode, it_exclude_fcode[].

    CREATE OBJECT ctl_alv
      EXPORTING
        i_parent = g_custom_container.

    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_excl_all.
    APPEND wa_exclude_fcode TO it_exclude_fcode.

    CALL METHOD ctl_alv->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        is_variant           = gs_variant
        it_toolbar_excluding = it_exclude_fcode
        i_save               = 'A'
      CHANGING
        it_fieldcatalog      = it_fieldcatalog
        it_outtab            = it_empregado[]
        it_sort              = it_sort.

    SET HANDLER: lcl_event_handler=>on_double_click FOR ctl_alv.

  ELSE.

    ls_stable-row = 'X'.
    ls_stable-col = 'X'.

    CALL METHOD ctl_alv->refresh_table_display
      EXPORTING
        is_stable = ls_stable
      EXCEPTIONS
        finished  = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
    ENDIF.

  ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT'.
      MESSAGE i000(o0) WITH 'Selecione centro de trabalho com double click'.
*      LEAVE TO SCREEN 0.
  ENDCASE.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_7098   text
*      -->P_7099   text
*      -->P_7100   text
*      -->P_7101   text
*      -->P_7102   text
*      -->P_7103   text
*      -->P_7104   text
*      -->P_7105   text
*      -->P_7106   text
*      -->P_7107   text
*      -->P_7108   text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog USING VALUE(p_colnum)
                                VALUE(p_fieldname)
                                VALUE(p_tabname)
                                VALUE(p_len)
                                VALUE(p_edit)
                                VALUE(p_icon)
                                VALUE(p_do_sum)
                                VALUE(p_header)
                                VALUE(p_emphasize)
                                VALUE(p_hotspot)
                                VALUE(p_ref_table)
                                VALUE(p_ref_field).

  DATA:  wa_fieldcatalog  TYPE lvc_s_fcat.
  wa_fieldcatalog-col_pos     = p_colnum.
  wa_fieldcatalog-fieldname   = p_fieldname.
  wa_fieldcatalog-tabname     = p_tabname.
  wa_fieldcatalog-outputlen   = p_len.
  wa_fieldcatalog-edit        = p_edit.
  wa_fieldcatalog-icon        = p_icon.
  wa_fieldcatalog-do_sum      = p_do_sum.
  wa_fieldcatalog-coltext     = p_header.
  wa_fieldcatalog-emphasize   = p_emphasize.
  wa_fieldcatalog-hotspot     = p_hotspot.
  wa_fieldcatalog-ref_table   = p_ref_table.
  wa_fieldcatalog-ref_table   = p_ref_field.
*  WA_FIELDCATALOG-CHECKTABLE  = P_CHECKTABLE.



  wa_fieldcatalog-hotspot     = p_ref_field.


*  WA_FIELDCATALOG-EXCP_CONDS  = P_EXCP_CONDS.

  gs_layout-excp_conds    = 'X'.
  gs_layout-zebra         = 'X'.
  gs_layout-sel_mode      = 'A'.
  gs_layout-cwidth_opt    = 'X'.     "  Otimizar colunas na tela
  gs_layout-totals_bef    = ''.

  APPEND wa_fieldcatalog TO it_fieldcatalog.


ENDFORM.                    " F_DEFINE_CONTAINER_HEADER

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_double_click.
    DATA: wa_afvc TYPE ty_aufk.
    DATA  wa_aufk TYPE ty_aufk.

    IF it_empregado IS NOT INITIAL.
      DATA: wa_empregado TYPE ty_hrp1001.
      READ TABLE it_empregado INTO wa_empregado INDEX e_row.

      CHECK sy-subrc IS INITIAL.
      zaponta-sname     = wa_empregado-sname.
      zaponta-work_cntr = wa_empregado-arbpl.
      zaponta-ktext     = wa_empregado-ktext.
      CLEAR wa_empregado.
      LEAVE TO SCREEN 0.
    ENDIF.

**  Begin of    #96115  FF  31.07.2023
    DATA: number        TYPE  bapi_alm_order_header_e-orderid,
          lt_operations TYPE TABLE OF  bapi_alm_order_operation_e,
          lt_return	    TYPE TABLE OF  bapiret2.

    number = |{ obj_main->header-orderid ALPHA = IN }|.

    CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
      EXPORTING
        number        = number
      TABLES
        et_operations = lt_operations
        return        = lt_return.

    DELETE lt_operations WHERE complete = 'X'.
**  End of    #96115  FF  31.07.2023

*    IF it_afvc[] IS NOT INITIAL.
    IF lt_operations[] IS NOT INITIAL.
      READ TABLE lt_operations INTO DATA(wa_oper) INDEX e_row.
      v_vornr = wa_oper-activity.
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDMETHOD.
ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

FORM fill_it_fieldcatalog_1 USING VALUE(p_colnum)
                                VALUE(p_fieldname)
                                VALUE(p_tabname)
                                VALUE(p_len)
                                VALUE(p_edit)
                                VALUE(p_icon)
                                VALUE(p_do_sum)
                                VALUE(p_header)
                                VALUE(p_emphasize)
                                VALUE(p_hotspot)
                                VALUE(p_ref_table)
                                VALUE(p_ref_field).

  DATA:  wa_fieldcatalog_1  TYPE lvc_s_fcat.
  wa_fieldcatalog_1-col_pos     = p_colnum.
  wa_fieldcatalog_1-fieldname   = p_fieldname.
  wa_fieldcatalog_1-tabname     = p_tabname.
  wa_fieldcatalog_1-outputlen   = p_len.
  wa_fieldcatalog_1-edit        = p_edit.
  wa_fieldcatalog_1-icon        = p_icon.
  wa_fieldcatalog_1-do_sum      = p_do_sum.
  wa_fieldcatalog_1-coltext     = p_header.
  wa_fieldcatalog_1-emphasize   = p_emphasize.
  wa_fieldcatalog_1-hotspot     = p_hotspot.
  wa_fieldcatalog_1-ref_table   = p_ref_table.
  wa_fieldcatalog_1-ref_table   = p_ref_field.
*  WA_FIELDCATALOG-CHECKTABLE  = P_CHECKTABLE.
  wa_fieldcatalog_1-hotspot     = p_ref_field.


*  WA_FIELDCATALOG-EXCP_CONDS  = P_EXCP_CONDS.

  gs_layout_1-excp_conds    = 'X'.
  gs_layout_1-zebra         = 'X'.
  gs_layout_1-sel_mode      = 'A'.
  gs_layout_1-cwidth_opt    = 'X'.     "  Otimizar colunas na tela
  gs_layout_1-totals_bef    = ''.

  APPEND wa_fieldcatalog_1 TO it_fieldcatalog_1.


ENDFORM.                    " F_DEFINE_CONTAINER_HEADER

FORM fill_it_fieldcatalog_2 USING VALUE(p_colnum)
                                VALUE(p_fieldname)
                                VALUE(p_tabname)
                                VALUE(p_len)
                                VALUE(p_edit)
                                VALUE(p_icon)
                                VALUE(p_do_sum)
                                VALUE(p_header)
                                VALUE(p_emphasize)
                                VALUE(p_hotspot)
                                VALUE(p_ref_table)
                                VALUE(p_ref_field).

  DATA:  wa_fieldcatalog_2  TYPE lvc_s_fcat.
  wa_fieldcatalog_2-col_pos     = p_colnum.
  wa_fieldcatalog_2-fieldname   = p_fieldname.
  wa_fieldcatalog_2-tabname     = p_tabname.
  wa_fieldcatalog_2-outputlen   = p_len.
  wa_fieldcatalog_2-edit        = p_edit.
  wa_fieldcatalog_2-icon        = p_icon.
  wa_fieldcatalog_2-do_sum      = p_do_sum.
  wa_fieldcatalog_2-coltext     = p_header.
  wa_fieldcatalog_2-emphasize   = p_emphasize.
  wa_fieldcatalog_2-hotspot     = p_hotspot.
  wa_fieldcatalog_2-ref_table   = p_ref_table.
  wa_fieldcatalog_2-ref_table   = p_ref_field.
*  WA_FIELDCATALOG-CHECKTABLE  = P_CHECKTABLE.
  wa_fieldcatalog_2-hotspot     = p_ref_field.


*  WA_FIELDCATALOG-EXCP_CONDS  = P_EXCP_CONDS.

  gs_layout_2-excp_conds    = 'X'.
  gs_layout_2-zebra         = 'X'.
  gs_layout_2-sel_mode      = 'A'.
  gs_layout_2-cwidth_opt    = 'X'.     "  Otimizar colunas na tela
  gs_layout_2-totals_bef    = ''.

  APPEND wa_fieldcatalog_2 TO it_fieldcatalog_2.


ENDFORM.                    " F_DEFINE_CONTAINER_HEADER

CLASS lcl_event_handler_nota IMPLEMENTATION.
  METHOD on_double_click.
    DATA: wa_empregado TYPE ty_hrp1001.
    READ TABLE it_empregado INTO wa_empregado INDEX e_row.

    CHECK sy-subrc IS INITIAL.
    zaponta-sname     = wa_empregado-sname.
    zaponta-work_cntr = wa_empregado-arbpl.
    zaponta-ktext     = wa_empregado-ktext.
    CLEAR wa_empregado.
    LEAVE TO SCREEN 0.
  ENDMETHOD.
ENDCLASS.
*&---------------------------------------------------------------------*
*&      Form  CONFIRM_SAIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM confirm_sair .
  DATA: p_respo TYPE c.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING        "TITLEBAR = 'Confirmar'
      text_question         = 'Sair da transação sem salvar as alterações?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      display_cancel_button = ' '
    IMPORTING
      answer                = p_respo.

  IF p_respo = 1.
**  Begin of    #96115  FF
    CLEAR zepm_aponta_cat_notas.
    CLEAR sy-ucomm.
*    LEAVE TO SCREEN 0.
    LEAVE PROGRAM.
** End of FF

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EXIBIR_ORDEM_NOTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exibir_ordem_nota .
  GET CURSOR FIELD w_cursor_field.
  CASE w_cursor_field.
    WHEN 'V_ORDEM'."Ordem Manuteção

      IF sy-subrc = 0.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING        "TITLEBAR = 'Confirmar'
            text_question         = 'Deseja acessar qual visão?'
            text_button_1         = 'Exibir ordem'
            text_button_2         = 'Exibir apto.hrs'
            display_cancel_button = 'X' "VALUE(DISPLAY_CANCEL_BUTTON) DEFAULT 'X'
          IMPORTING
            answer                = p_resp.

        CASE p_resp.
          WHEN '1'.
            IF v_ordem IS NOT INITIAL.
              SET PARAMETER ID 'ANR' FIELD v_ordem.
              CALL TRANSACTION 'IW33' AND SKIP FIRST SCREEN .
            ENDIF.

          WHEN '2'.

            CLEAR ti_bdcdata.
            PERFORM f_bdc_data USING:
'        '      '0000'  'T'   '            '      'BS AA X   F',
'RIAFRU20'      '1000'  'X'   '            '        '           ',
'        '      '0000'  ' '   'BDC_CURSOR  '      'VARIANT    ',
'        '      '0000'  ' '   'BDC_OKCODE  '      '=ONLI      ',
'        '      '0000'  ' '   'DY_IAR      '      'X          ',
'        '      '0000'  ' '   'DY_ABG      '      'X          ',
'        '      '0000'  ' '   'AUFNR_O-LOW '      v_ordem      ,
'        '      '0000'  ' '   'ERSDA_C-LOW '      '           ',
'        '      '0000'  ' '   'ERSDA_C-HIGH'      '           ',
'        '      '0000'  ' '   'VARIANT     '      '/APONT. M.O',
'        '      '0000'  ' '   'NO_CANC     '      'X',      "FF #96115
'SAPMSSY0'      '0120'  'X'   '            '        '           ',
*'        '      '0000'  ' '   'BDC_CURSOR  '      '04/03      ',
*'        '      '0000'  ' '   'BDC_OKCODE  '      '=BACK      ', "FF #96115
'RIAFRU20'      '1000'  'X'   '            '        '           ',
'        '      '0000'  ' '   'BDC_OKCODE  '      '/EE        ',
'        '      '0000'  ' '   'BDC_CURSOR  '      'SELSCHEM   '.


            PERFORM zf_call_transaction USING 'IW47' CHANGING p_erro.
            IF p_erro IS NOT INITIAL.
              MESSAGE TEXT-007 TYPE 'S' DISPLAY LIKE 'E'.
            ENDIF.


          WHEN OTHERS.
            LEAVE TO SCREEN 0100.
        ENDCASE.
      ENDIF.



    WHEN 'V_NOTA'.
      IF v_nota IS NOT INITIAL.
        SET PARAMETER ID 'IQM' FIELD v_nota.
        CALL TRANSACTION 'IW23' AND SKIP FIRST SCREEN .

        WAIT UP TO 5 SECONDS.

        v_ordem = |{ v_ordem ALPHA = IN }|.
*        V_ORDEM = '000011001000'.
        FREE obj_main->header.
        CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
          EXPORTING
            number    = v_ordem
          IMPORTING
            es_header = obj_main->header
          TABLES
            return    = obj_main->it_return.

        DATA: catalog_profile TYPE bapi10011e,
              return1         TYPE bapireturn,
              codes           TYPE TABLE OF  bapi10011t.

        CALL FUNCTION 'BAPI_SERVNOT_GETCATALOGPROFIL'
          EXPORTING
            number          = obj_main->header-notif_no
            language        = sy-langu
          IMPORTING
            catalog_profile = catalog_profile
            return          = return1
          TABLES
            codes           = codes.

        CHECK obj_main->get_item( obj_main->header-notif_no ) IS INITIAL.

        obj_main->set_item( ).

*        OBJ_MAIN->alv1->refresh_table_display( is_stable = OBJ_MAIN->stable ).
        obj_main->alv2->refresh_table_display( EXPORTING is_stable = obj_main->stable ).
        obj_main->alv3->refresh_table_display( EXPORTING is_stable = obj_main->stable ).
        obj_main->alv4->refresh_table_display( EXPORTING is_stable = obj_main->stable ).
        obj_main->alv5->refresh_table_display( EXPORTING is_stable = obj_main->stable ).
        obj_main->alv6->refresh_table_display( EXPORTING is_stable = obj_main->stable ).
      ENDIF.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0400 INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0400  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0400 OUTPUT.
  SET PF-STATUS 'T004'.
  SET TITLEBAR 'T005'.

*  FREE G_CUSTOM_CONTAINER_1.
* Adicionando Logo Marca no Cabeçalho
  IF g_custom_container_1 IS INITIAL.

    CREATE OBJECT g_custom_container_1
      EXPORTING
        container_name              = 'CONTAINER_2'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    PERFORM fill_it_fieldcatalog_1 USING:
        2  'PERNR'  'TL_AFRU'  '10'   ' '  ' '  ' '  'Empregado               '  ''  ''  'AFRU '  ' ',
        1  'WERKS'  'TL_AFRU'  '20'   ' '  ' '  ' '  'Centro                  '  ''  ''  'AFRU '  ' ',
        3  'BUDAT'  'TL_AFRU'  '20'   ' '  ' '  ' '  'Data lançamento         '  ''  ''  'AFRU '  ' ',
        4  'ISDD '  'TL_AFRU'  '20'   ' '  ' '  ' '  'Data Inic               '  ''  ''  'AFRU '  ' ',
        5  'ISDZ '  'TL_AFRU'  '20'   ' '  ' '  ' '  'Hora Inic               '  ''  ''  'AFRU '  ' ',
        6  'IEDD '  'TL_AFRU'  '20'   ' '  ' '  ' '  'Data Fim                '  ''  ''  'AFRU '  ' ',
        7  'IEDZ '  'TL_AFRU'  '20'   ' '  ' '  ' '  'Hora Fim                '  ''  ''  'AFRU '  ' ',
        8  'AUFNR'  'TL_AFRU'  '20'   ' '  ' '  ' '  'Ordem                   '  ''  ''  'AFRU '  ' ',
        9  'VORNR'  'TL_AFRU'  '20'   ' '  ' '  ' '  'Operaçao                '  ''  ''  'AFRU '  ' ',
       10  'ISMNW'  'TL_AFRU'  '20'   ' '  ' '  ' '  'Trabalho Real           '  ''  ''  'AFRU '  ' ',
       11  'ISMNE'  'TL_AFRU'  '20'   ' '  ' '  ' '  'Unidade de trabalho     '  ''  ''  'AFRU '  ' '.


    gs_layout_1-sel_mode   = 'A'.
    gs_layout_1-cwidth_opt = 'X'.
    CLEAR: it_exclude_fcode_1, it_exclude_fcode_1[].

    CREATE OBJECT ctl_alv_1
      EXPORTING
        i_parent = g_custom_container_1.

    wa_exclude_fcode_1 = cl_gui_alv_grid=>mc_fc_excl_all.
    APPEND wa_exclude_fcode_1 TO it_exclude_fcode_1.

    CALL METHOD ctl_alv_1->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout_1
        is_variant           = gs_variant_1
        it_toolbar_excluding = it_exclude_fcode_1
        i_save               = 'A'
      CHANGING
        it_fieldcatalog      = it_fieldcatalog_1
        it_outtab            = tl_afru[]
*       IT_OUTTAB            = T_AFRU[]
        it_sort              = it_sort_1.

  ELSE.

    ls_stable_1-row = 'X'.
    ls_stable_1-col = 'X'.

    CALL METHOD ctl_alv_1->refresh_table_display
      EXPORTING
        is_stable = ls_stable_1
      EXCEPTIONS
        finished  = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
    ENDIF.

  ENDIF.




ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  HAB_ENC_ORDEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM hab_enc_ordem .

  DATA: p_respo TYPE c.

  IF v_conf_enc IS NOT INITIAL.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING        "TITLEBAR = 'Confirmar'
        text_question         = 'Deseja desmarcar encerramento da ordem?'
        text_button_1         = 'Sim'
        text_button_2         = 'Não'
        display_cancel_button = ' '
      IMPORTING
        answer                = p_respo.

    IF p_respo = 1.
      CLEAR v_conf_enc.
    ENDIF.
  ELSE.
    v_conf_enc = '@01@'."ICON_CHECKED.
    MODIFY SCREEN.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HAB_ENC_NOTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM hab_enc_nota .
  DATA: p_respo TYPE c.
  IF zaponta-aufnr IS NOT INITIAL.
    IF v_conf_nota IS NOT INITIAL.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING        "TITLEBAR = 'Confirmar'
          text_question         = 'Deseja desmarcar encerramento da nota?'
          text_button_1         = 'Sim'
          text_button_2         = 'Não'
          display_cancel_button = ' '
        IMPORTING
          answer                = p_respo.

      IF p_respo = 1.
        CLEAR v_conf_nota.
      ENDIF.
    ELSE.
      LOOP AT SCREEN.
        CASE screen-name.
          WHEN 'V_ENC_NOTA'.
*            SCREEN-ACTIVE = 0.
*        SCREEN-INPUT = 0.
            v_conf_nota = '@01@'."ICON_CHECKED.
            MODIFY SCREEN.
        ENDCASE.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ELIM_LINHA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM elim_linha.

  DATA: w_opera TYPE ty_zaponta.
  DATA: p_resp,
  lv_msg TYPE bapi_msg.


  CLEAR: it_select_rows[], wa_select_rows, w_opera.

  IF it_aponta IS NOT INITIAL.
    CALL METHOD obj_main->alv1->get_selected_rows
      IMPORTING
        et_index_rows = it_select_rows.


    IF it_select_rows[] IS NOT INITIAL.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING        "TITLEBAR = 'Confirmar'
          text_question         = 'Deseja realmente excluir a linha?'
          text_button_1         = 'Sim'
          text_button_2         = 'Não'
          display_cancel_button = ' '
        IMPORTING
          answer                = p_resp.

      IF p_resp =< 1.

        LOOP AT it_select_rows INTO wa_select_rows.
          LOOP AT it_aponta ASSIGNING FIELD-SYMBOL(<faponta>).
            IF sy-tabix = wa_select_rows-index.
              <faponta>-marc = 'X'.
            ENDIF.
          ENDLOOP.
        ENDLOOP.

        DELETE it_aponta WHERE marc EQ 'X'.
        IF sy-subrc = 0.
          MESSAGE s000(o0) WITH 'Informação excluida com sucesso' DISPLAY LIKE 'S'.
        ENDIF.

        obj_main->alv1->refresh_table_display( is_stable = obj_main->stable ).

      ENDIF.

    ELSE.
      MESSAGE i026(sv)." WITH 'Selecione uma linha para excluir'.
    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MODIF_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modif_screen .

**  Begin of   "FF #96115
  IF v_ordem IS NOT INITIAL.

    DATA(lv_hr00) = 'HR00'. "Ordem para apontamento de horas improdutivas

    SELECT SINGLE auart
      FROM caufv
      INTO @DATA(dummy)
      WHERE aufnr = @v_ordem
        AND auart = @lv_hr00.

    IF sy-subrc = 0.
      LOOP AT SCREEN.
        IF screen-group2 = 'HR'. "Parte Obj, sintoma dano e causa
          screen-input = '0'. "Não editável
          screen-invisible = '1'. "Não exibir
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
** End of FF

**  Begin of    #96115  FF  07.03.2023
  READ TABLE it_zpmt0012 WITH KEY encerrar = 'X' INTO DATA(wa_0012).
  IF sy-subrc <> 0.
    CLEAR wa_0012.
  ENDIF.
** End of FF  07.03.2023
  LOOP AT SCREEN.
    CASE  screen-name.
      WHEN 'V_ENC_ORDEM'.
        IF it_aponta IS INITIAL OR wa_0012-encerrar = ' '. "#96115  FF  07.03.2023

          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.

      WHEN 'V_ENC_NOTA'.
        IF obj_main->header-notif_no IS INITIAL OR wa_0012-encerrar = ' '. "#96115  FF  07.03.2023.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.

      WHEN 'V_ORDEM'.
        IF it_aponta IS NOT INITIAL.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.

*      WHEN 'V_VORNR'.
*        IF it_aponta IS NOT INITIAL.
*          screen-input = 0.
*          MODIFY SCREEN.
*        ENDIF.
    ENDCASE.
  ENDLOOP.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0500 INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT'.
*      MESSAGE I000(O0) WITH 'Selecione uma operação com double click'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0500  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0500 OUTPUT.

  SET PF-STATUS 'T005'.
  SET TITLEBAR 'T006'.

**  Begin of    #96115  FF  29.03.2023
  DATA: lv_number TYPE  bapi_alm_order_header_e-orderid,
        lt_oper   TYPE TABLE OF  bapi_alm_order_operation_e,
        lt_ret    TYPE TABLE OF bapiret2.

  lv_number = |{ obj_main->header-orderid ALPHA = IN }|.

  CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
    EXPORTING
      number        = lv_number
    TABLES
      et_operations = lt_oper
      return        = lt_ret.

  DELETE lt_oper WHERE complete = 'X'.

  IF v_vornr IS NOT INITIAL.
    DELETE lt_oper WHERE activity <> v_vornr.
  ENDIF.

  IF obj_main->t_aufk IS NOT INITIAL.
    v_aufnr = obj_main->header-orderid.

    LOOP AT obj_main->t_aufk ASSIGNING FIELD-SYMBOL(<w_aufk>).
      v_ktext = <w_aufk>-ktext.
    ENDLOOP.

    FREE it_afvc[].

    MOVE-CORRESPONDING obj_main->t_aufk TO it_afvc[].

    IF g_custom_container_2 IS INITIAL.

      CREATE OBJECT g_custom_container_2
        EXPORTING
          container_name              = 'CONTAINER_3'
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5.

      PERFORM fill_it_fieldcatalog_2 USING:
          2  'ACTIVITY'    'LT_OPERATIONS '  '05'   ' '  ' '  ' '  'Operação          '  ''  ''  'BAPI_ALM_ORDER_OPERATION_E '  ' ',
          1  'WORK_CNTR'   'LT_OPERATIONS '  '20'   ' '  ' '  ' '  'Cen Trab          '  ''  ''  'BAPI_ALM_ORDER_OPERATION_E '  ' ',
          3  'PLANT'       'LT_OPERATIONS '  '10'   ' '  ' '  ' '  'Centro            '  ''  ''  'BAPI_ALM_ORDER_OPERATION_E '  ' ',
          4  'DESCRIPTION' 'LT_OPERATIONS '  '40'   ' '  ' '  ' '  'Txt.breve operação'  ''  ''  'BAPI_ALM_ORDER_OPERATION_E '  ' ',
          5  'ASSEMBLY'    'LT_OPERATIONS '  '17'   ' '  ' '  ' '  'Conjunto          '  ''  ''  'BAPI_ALM_ORDER_OPERATION_E '  ' '.

      gs_layout_2-sel_mode   = 'A'.
      gs_layout_2-cwidth_opt = 'X'.
      CLEAR: it_exclude_fcode_2, it_exclude_fcode_2[].

      CREATE OBJECT ctl_alv_2
        EXPORTING
          i_parent = g_custom_container_2.

      wa_exclude_fcode_2 = cl_gui_alv_grid=>mc_fc_excl_all.
      APPEND wa_exclude_fcode_2 TO it_exclude_fcode_2.

      CALL METHOD ctl_alv_2->set_table_for_first_display
        EXPORTING
          is_layout            = gs_layout_2
          is_variant           = gs_variant_2
          it_toolbar_excluding = it_exclude_fcode_2
          i_save               = 'A'
        CHANGING
          it_fieldcatalog      = it_fieldcatalog_2
          it_outtab            = lt_oper
          it_sort              = it_sort_2.

      SET HANDLER: lcl_event_handler=>on_double_click FOR ctl_alv_2.

    ELSE.

      ls_stable_2-row = 'X'.
      ls_stable_2-col = 'X'.

      CALL METHOD ctl_alv_2->refresh_table_display
        EXPORTING
          is_stable = ls_stable_2
        EXCEPTIONS
          finished  = 1
          OTHERS    = 2.

      IF sy-subrc <> 0.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  V_ORDEM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE v_ordem INPUT.
*  V_VORNR = OBJ_MAIN->HEADER-ORDERID.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  V_KTEXT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE v_ktext INPUT.
*  LOOP AT OBJ_MAIN->T_AUFK ASSIGNING FIELD-SYMBOL(<W_AUFK>).
*    V_KTEXT = <W_AUFK>-KTEXT.
*  ENDLOOP.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  CHECK_REQ_AND_PED_PENDENTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_HEARDER_ORDERID  text
*----------------------------------------------------------------------*
FORM check_req_and_ped_pendente  USING p_header-orderid.

  IF sy-tcode = 'ZPM0047'."'IW32' OR
* AND SY-UCOMM = 'ARCH'.
    TYPES: BEGIN OF ty_ebkn,          " Classificação contábil da requisição de compra
             banfn TYPE ebkn-banfn,  " Nº requisição de compra
             bnfpo TYPE ebkn-bnfpo,  " Nº do item da requisição de compra
             aufnr TYPE ebkn-aufnr,  " Nº ordem
           END OF ty_ebkn,

           BEGIN OF ty_eban,         " Requisições de compras da ordem
             banfn TYPE eban-banfn,  " Numero da requisição
             bnfpo TYPE eban-bnfpo,  " Item da requisiçãp
             statu TYPE eban-statu,  " Status do item
             frgkz TYPE eban-frgkz,  " Status da aprovação
             matnr TYPE eban-matnr,  " Numero do material
             arsnr TYPE eban-arsnr,  " Numero da reserva
             arsps TYPE eban-arsps,  " Item da reserva
             ebakz TYPE eban-ebakz,  " Requisição de compra concluída
             loekz TYPE eban-loekz,  " Código de eliminação no documento de compras
             wepos TYPE eban-wepos,  " Código de entrada de mercadorias
             ebeln TYPE eban-ebeln,  " Nº pedido
             ebelp TYPE eban-ebelp,  " Nº item do pedido
             pstyp TYPE eban-pstyp,  " Ctg.item no documento compra
             frgst TYPE eban-frgst,  " Estratégia de liberação na requisição de compra
             menge TYPE eban-menge,  " Quantidade da requisição de compra
             bsmng TYPE eban-bsmng,  " Quantidade pedida da ReqC
             ztipo TYPE c,           " Tipo de requisição ( M - Material, S - Serviço )
           END OF ty_eban,

           BEGIN OF ty_ekpo,         " Itens pedido de compra.
             ebeln  TYPE ekpo-ebeln,  " Nº do documento de compras
             ebelp  TYPE ekpo-ebelp,  " Nº item do documento de compra
             loekz  TYPE ekpo-loekz,  " Código de eliminação
             elikz  TYPE ekpo-elikz,  " Código de remessa final
             pstyp  TYPE ekpo-pstyp,  " Ctg.item no documento compra
             packno TYPE ekpo-packno, " Nº pacote
           END OF ty_ekpo,

           BEGIN OF ty_esll,          " Linhas do pacote de serviços
             packno     TYPE esll-packno,     " Nº pacote
             sub_packno TYPE esll-sub_packno, " Nº do subpacote
             menge      TYPE esll-menge,      " Qtd.com símbolo +/-
             act_menge  TYPE esll-act_menge,  " Pedido: qtd.registrada
           END OF ty_esll.



    DATA: t_eban      TYPE TABLE OF ty_eban,
          t_ebkn      TYPE TABLE OF ty_ebkn,
          t_esll      TYPE TABLE OF ty_esll,
          t_esll_sub  TYPE TABLE OF ty_esll,
          w_eban      TYPE ty_eban,
          w_ebkn      TYPE ty_ebkn,
          w_ekpo      TYPE ty_ekpo,
          w_esll      TYPE ty_esll,
          w_esll_sub  TYPE ty_esll,
          l_resul     TYPE eket-menge,
          l_len       TYPE i,
          l_msg(100)  TYPE c,
          l_banfn(82) TYPE c,
          l_ebeln(82) TYPE c.

    CLEAR: t_req_pend, t_ped_pend.
    REFRESH: t_req_pend[], t_ped_pend[].

* Verifica se existem requisições vinculadas a ordem.
    SELECT banfn bnfpo aufnr INTO TABLE t_ebkn
      FROM ebkn
      WHERE aufnr = p_header-orderid.

    "Se não existir requisição vinculada a ordem - SAIR.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.


* Verificar se existe requisição aprovada ou item eliminado.
    SELECT banfn bnfpo statu frgkz matnr arsnr arsps ebakz loekz wepos
           ebeln ebelp pstyp frgst menge bsmng
      INTO CORRESPONDING FIELDS OF TABLE t_eban
      FROM eban
      FOR ALL ENTRIES IN t_ebkn
      WHERE banfn EQ t_ebkn-banfn
      AND   bnfpo EQ t_ebkn-bnfpo.

    IF t_eban IS INITIAL.
      EXIT.     " Não existe requisição .
    ENDIF.

    LOOP AT t_eban INTO w_eban.

      IF w_eban-frgst IS INITIAL.
        CONTINUE.
      ENDIF.
* se requisição aprovada e item não eliminado
      IF w_eban-frgkz EQ 'X'.              " Requisição aprovada.
        CONTINUE.
      ELSEIF w_eban-loekz IS NOT INITIAL.  " Item não eliminado.
        CONTINUE.
      ENDIF.

* se Requisição pendente - EBAN-STATU – Status de processamento.
* Observar as seguintes regras:
* Para “N” – Não processado, ou “A” Sol. Cotação criada
* bloquear o encerramento técnico.
      IF w_eban-statu = 'N' OR         " Não processada.
         w_eban-statu = 'A'.           " Sol. cotação
        t_req_pend-banfn = w_eban-banfn.
        APPEND t_req_pend .
        CONTINUE.
      ENDIF.

* Para EBAN-STATU = “B” – Pedido criado verificar pedido.
      IF w_eban-statu NE 'B'.          " Pedido não criado
        CONTINUE.
      ENDIF.

* Se qtde solicitada ReqC menos Qtde pedida na ReqC diferente de zero
* enviar msg de Requisição Pendente.
      l_resul = w_eban-menge - w_eban-bsmng.

      IF l_resul NE 0.
        t_req_pend-banfn = w_eban-banfn.
        APPEND t_req_pend .
        CLEAR l_resul.
        CONTINUE.
      ENDIF.

* Pedido criado:
* verificar na tabela EKPO as seguintes condições:
* 1 - EKPO-LOEKZ – Código de eliminação = branco
* 2 - EKET-MENGE – Qtde. da divisão – EKET-WEMNG – Fornecido  # de 0, envia msg.
      SELECT SINGLE ebeln ebelp loekz elikz pstyp packno INTO w_ekpo
        FROM ekpo
        WHERE ebeln EQ w_eban-ebeln
        AND   ebelp EQ w_eban-ebelp.

      IF w_ekpo-loekz IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      IF w_ekpo-pstyp NE '9'.

*    Se ekpo-elikz for branco.
*    envia mensagem de pedido pendente.
        IF w_ekpo-elikz IS INITIAL.
          t_ped_pend-ebeln = w_ekpo-ebeln.
          APPEND t_ped_pend .
          CONTINUE.
        ENDIF.
      ELSE.
        SELECT packno sub_packno menge act_menge
          FROM esll
          INTO TABLE t_esll
          WHERE packno = w_ekpo-packno.
        IF sy-subrc NE 0.
          CONTINUE.
        ENDIF.

        LOOP AT t_esll INTO w_esll.
          SELECT packno sub_packno menge act_menge
          FROM esll
          INTO TABLE t_esll_sub
          WHERE packno = w_esll-sub_packno.
          IF sy-subrc NE 0.
            CONTINUE.
          ENDIF.
          LOOP AT t_esll_sub INTO w_esll_sub.
            l_resul = w_esll_sub-menge - w_esll_sub-act_menge.
            IF l_resul NE 0.
              t_ped_pend-ebeln = w_ekpo-ebeln.
              APPEND t_ped_pend .
            ENDIF.
          ENDLOOP.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

*  select single ebeln ebelp etenr menge wemng
*    from eket
*    into w_eket
*    where ebeln  = w_ekpo-ebeln
*    and   ebelp  = w_ekpo-ebelp.
* Se EKET-MENGE – EKET-WEMNG é diferente de zero
* envia mensagem de pedido pendente.
*    l_resul = w_eket-menge - w_eket-wemng.
*    if l_resul ne 0.
*      t_ped_pend-ebeln = w_eket-ebeln.
*      append t_ped_pend .
*      continue.
*    endif.
*  endif.
*endloop.

    IF t_req_pend IS NOT INITIAL.
      SORT t_req_pend BY banfn.
      DELETE ADJACENT DUPLICATES FROM t_req_pend COMPARING ALL FIELDS.
      LOOP AT t_req_pend.
        CONCATENATE t_req_pend-banfn ',*' l_banfn INTO l_banfn.
      ENDLOOP.
      l_len = strlen( l_banfn ).
      l_len = l_len - 2.
      TRANSLATE l_banfn USING '* '.
      TRANSLATE l_banfn+l_len USING ',.'.
      CONCATENATE '>>> Requisições Pendentes' l_banfn INTO l_msg
            SEPARATED BY space.
      MESSAGE l_msg TYPE 'I'.
    ENDIF.

    IF t_ped_pend IS NOT INITIAL.
      SORT t_ped_pend BY ebeln.
      DELETE ADJACENT DUPLICATES FROM t_ped_pend COMPARING ebeln.
      LOOP AT t_ped_pend.
        CONCATENATE t_ped_pend-ebeln ',*' l_ebeln INTO l_ebeln.
      ENDLOOP.
      l_len = strlen( l_ebeln ).
      l_len = l_len - 2.
      TRANSLATE l_ebeln USING '* '.
      TRANSLATE l_ebeln+l_len USING ',.'.
      CONCATENATE '>>> Pedido(s) Pendente(s)' l_ebeln INTO l_msg
                SEPARATED BY space.
      MESSAGE l_msg TYPE 'I'.
    ENDIF.

*    IF T_REQ_PEND IS NOT INITIAL OR
*       T_PED_PEND IS NOT INITIAL.
*      MESSAGE I007 WITH P_HEADER-ORDERID DISPLAY LIKE 'E'.
*    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_BLOQ_ORDEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_bloq_ordem USING v_ordem CHANGING sy-subrc.

  DATA: tl_enq     TYPE TABLE OF seqg3 WITH HEADER LINE,
        wl_num_enq TYPE sy-tabix,
        wl_arg     TYPE seqg3-garg,
        coruf      TYPE coruf,
        tco01      TYPE tco01,
        aufnr      TYPE seqg3-garg.

  CLEAR sy-subrc.
  wl_arg  = |{ sy-mandt }{ v_ordem }|.
  v_ordem = |{ v_ordem ALPHA = IN }|.
  tco01-autyp = '30'.
  coruf-aufnr = v_ordem.

  CALL FUNCTION 'CO_RU_ORDER_LOCK'
    EXPORTING
      aufnr_imp            = coruf-aufnr
      autyp_imp            = tco01-autyp
    EXCEPTIONS
      order_already_locked = 1.

  CASE sy-subrc.
    WHEN 1.
      CLEAR sy-subrc.
      sy-subrc = 1.
      MESSAGE e469(co) WITH coruf-aufnr sy-msgv2
                       RAISING order_already_locked.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WHEN  text
*      -->P_OTHERS  text
*----------------------------------------------------------------------*
FORM f_bdc_data  USING p_program p_dynpro p_start p_fnam p_fval.

  APPEND VALUE #(
                program   = p_program
                dynpro    = p_dynpro
                dynbegin  = p_start
                fnam      = p_fnam
                fval      = p_fval
  ) TO ti_bdcdata.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2527   text
*      <--P_P_ERRO  text
*----------------------------------------------------------------------*
FORM zf_call_transaction USING p_trans CHANGING p_erro.

  CONSTANTS: c_msgid LIKE it_msg-msgid VALUE 'F5',
             c_msgnr LIKE it_msg-msgnr VALUE '312',
             c_msgne LIKE it_msg-msgnr VALUE '539'.

  DATA: wl_cont    TYPE sy-tabix,
        wl_mode(1).

*  FREE IT_MSG .
  CLEAR wl_mode.
  CLEAR wl_cont.
  CLEAR it_msg.
  wl_mode = 'E'.

  CALL TRANSACTION p_trans USING ti_bdcdata
                           MODE wl_mode
                           MESSAGES INTO it_msg.
*  COMMIT WORK.
*  WAIT UP TO 10 SECONDS.

  CLEAR: wl_cont.

  IF line_exists( it_msg[ msgtyp = 'A' ] ).
    p_erro = abap_true.
  ELSE.
    IF line_exists( it_msg[ msgtyp = 'E' ] ).
      p_erro = abap_true.
    ENDIF.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
FORM user_ok_tc USING    p_tc_name TYPE dynfnam
                         p_table_name
                         p_mark_name
                CHANGING p_ok      LIKE sy-ucomm.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA: l_ok     TYPE sy-ucomm,
        l_offset TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
  SEARCH p_ok FOR p_tc_name.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
  l_offset = strlen( p_tc_name ) + 1.
  l_ok = p_ok+l_offset.
*&SPWIZARD: execute general and TC specific operations                 *
  CASE l_ok.
    WHEN 'INSR'.                      "insert row
      PERFORM fcode_insert_row USING    p_tc_name
                                        p_table_name.
      CLEAR p_ok.

    WHEN 'DELE'.                      "delete row
      PERFORM fcode_delete_row USING    p_tc_name
                                        p_table_name
                                        p_mark_name.
      CLEAR p_ok.

    WHEN 'P--' OR                     "top of list
         'P-'  OR                     "previous page
         'P+'  OR                     "next page
         'P++'.                       "bottom of list
      PERFORM compute_scrolling_in_tc USING p_tc_name
                                            l_ok.
      CLEAR p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
    WHEN 'MARK'.                      "mark all filled lines
      PERFORM fcode_tc_mark_lines USING p_tc_name
                                        p_table_name
                                        p_mark_name   .
      CLEAR p_ok.

    WHEN 'DMRK'.                      "demark all filled lines
      PERFORM fcode_tc_demark_lines USING p_tc_name
                                          p_table_name
                                          p_mark_name .
      CLEAR p_ok.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

  ENDCASE.

ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_insert_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_lines_name       LIKE feld-name.
  DATA l_selline          LIKE sy-stepl.
  DATA l_lastline         TYPE i.
  DATA l_line             TYPE i.
  DATA l_table_name       LIKE feld-name.
  FIELD-SYMBOLS <tc>                 TYPE cxtab_control.
  FIELD-SYMBOLS <table>              TYPE STANDARD TABLE.
  FIELD-SYMBOLS <lines>              TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_lines_name.
  ASSIGN (l_lines_name) TO <lines>.

*&SPWIZARD: get current line                                           *
  GET CURSOR LINE l_selline.
  IF sy-subrc <> 0.                   " append line to table
    l_selline = <tc>-lines + 1.
*&SPWIZARD: set top line                                               *
    IF l_selline > <lines>.
      <tc>-top_line = l_selline - <lines> + 1 .
    ELSE.
      <tc>-top_line = 1.
    ENDIF.
  ELSE.                               " insert line into table
    l_selline = <tc>-top_line + l_selline - 1.
    l_lastline = <tc>-top_line + <lines> - 1.
  ENDIF.
*&SPWIZARD: set new cursor line                                        *
  l_line = l_selline - <tc>-top_line + 1.

*&SPWIZARD: insert initial line                                        *
  INSERT INITIAL LINE INTO <table> INDEX l_selline.
  <tc>-lines = <tc>-lines + 1.
*&SPWIZARD: set cursor                                                 *
  SET CURSOR LINE l_line.

ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_delete_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name
                       p_mark_name   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
  DESCRIBE TABLE <table> LINES <tc>-lines.

  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    IF <mark_field> = 'X'.
      DELETE <table> INDEX syst-tabix.
      IF sy-subrc = 0.
        <tc>-lines = <tc>-lines - 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
FORM compute_scrolling_in_tc USING    p_tc_name
                                      p_ok.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_tc_new_top_line     TYPE i.
  DATA l_tc_name             LIKE feld-name.
  DATA l_tc_lines_name       LIKE feld-name.
  DATA l_tc_field_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <lines>      TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.
*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_tc_lines_name.
  ASSIGN (l_tc_lines_name) TO <lines>.


*&SPWIZARD: is no line filled?                                         *
  IF <tc>-lines = 0.
*&SPWIZARD: yes, ...                                                   *
    l_tc_new_top_line = 1.
  ELSE.
*&SPWIZARD: no, ...                                                    *
    CALL FUNCTION 'SCROLLING_IN_TABLE'
      EXPORTING
        entry_act      = <tc>-top_line
        entry_from     = 1
        entry_to       = <tc>-lines
        last_page_full = 'X'
        loops          = <lines>
        ok_code        = p_ok
        overlapping    = 'X'
      IMPORTING
        entry_new      = l_tc_new_top_line
      EXCEPTIONS
*       NO_ENTRY_OR_PAGE_ACT  = 01
*       NO_ENTRY_TO    = 02
*       NO_OK_CODE_OR_PAGE_GO = 03
        OTHERS         = 0.
  ENDIF.

*&SPWIZARD: get actual tc and column                                   *
  GET CURSOR FIELD l_tc_field_name
             AREA  l_tc_name.

  IF syst-subrc = 0.
    IF l_tc_name = p_tc_name.
*&SPWIZARD: et actual column                                           *
      SET CURSOR FIELD l_tc_field_name LINE 1.
    ENDIF.
  ENDIF.

*&SPWIZARD: set the new top line                                       *
  <tc>-top_line = l_tc_new_top_line.


ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_mark_lines USING p_tc_name
                               p_table_name
                               p_mark_name.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_demark_lines USING p_tc_name
                                 p_table_name
                                 p_mark_name .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = space.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  CODE_PF4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM code_pf4 USING p_cat.

  TABLES viqmel.

  DATA: lkatart LIKE qkat-katalogart.
  DATA: lpick VALUE 'X'.               "Einfachauswahl (ein: 'X')
  DATA: lretn VALUE ' '.               "Anzeigemodus (ein: 'X')
  DATA: h_code LIKE viqmfe-fecod VALUE '*' .
  DATA: h_field(20) VALUE ' '.
  DATA: l_subrc LIKE sy-subrc.
  DATA: ldisp      TYPE c VALUE ''.
  DATA: cdgrp      LIKE qmfe-fegrp.

  DATA number LIKE  bapi2080_nothdre-notif_no.
  DATA notifheader_export LIKE  bapi2080_nothdre.

  IF v_nota IS INITIAL.
    LOOP AT lt_operations INTO DATA(wa_oper) WHERE notif_no IS NOT INITIAL.
      number = |{ wa_oper-notif_no ALPHA = IN }|.
      EXIT.
    ENDLOOP.
  ELSE.

    number = |{ v_nota ALPHA = IN } |.
  ENDIF.

  CALL FUNCTION 'BAPI_ALM_NOTIF_GET_DETAIL'
    EXPORTING
      number             = number
    IMPORTING
      notifheader_export = notifheader_export.

  CHECK notifheader_export IS NOT INITIAL.

  viqmel-qmart = notifheader_export-notif_type.

  "Busca tipo de catálogo
  IF p_cat = 'OTKAT'. "Parte Obejto
    SELECT SINGLE otkat FROM tq80
      INTO lkatart
      WHERE qmart =  viqmel-qmart.
  ELSEIF p_cat = 'FEKAT'. "Sintoma Dano
    SELECT SINGLE fekat FROM tq80
      INTO lkatart
      WHERE qmart =  viqmel-qmart.
  ELSEIF p_cat = 'URKAT'. "Causa
    SELECT SINGLE urkat FROM tq80
      INTO lkatart
      WHERE qmart =  viqmel-qmart.
  ELSEIF p_cat = 'MFKAT'. "Códigos de ação
    SELECT SINGLE mfkat FROM tq80
      INTO lkatart
      WHERE qmart =  viqmel-qmart.
  ENDIF.
  IF sy-subrc <> 0.
    CLEAR lkatart.
  ENDIF.

  "Busca catálogo no equipamento
  SELECT SINGLE * FROM equz
    INTO @DATA(ls_equz)
    WHERE equnr = @notifheader_export-equipment
      AND rbnr <> @space.

  IF sy-subrc = 0.
    viqmel-rbnr = ls_equz-rbnr.

  ELSE.

    "Busca catálogo no local de instalação.
    SELECT SINGLE * FROM iflot
      INTO @DATA(ls_iflot)
      WHERE tplnr = @notifheader_export-funct_loc
        AND rbnr <> @space.

    IF sy-subrc = 0.
      viqmel-rbnr =   ls_iflot-rbnr.
    ELSE.

      "Busca catálogo na nota
      viqmel-rbnr = notifheader_export-catprofile.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'CATALOGUE_SELECTION'
    EXPORTING
      rbnr                    = viqmel-rbnr
      qmart                   = viqmel-qmart
    TABLES
      t_riwo020tab            = g_riwo020tab
    EXCEPTIONS
      cdgrp_not_valid_pattern = 1
      OTHERS                  = 2.

  CLEAR g_codegrptab.
  REFRESH g_codegrptab.
  LOOP AT g_riwo020tab WHERE qkatart = lkatart.
    MOVE g_riwo020tab-qcodegrp TO g_codegrptab-codegruppe.
    APPEND g_codegrptab.
  ENDLOOP.

  "FF - Ajuste catálogo "A" #134401 - inicio
  IF lkatart = 'A'. "Ação
    SELECT SINGLE auart FROM caufv
    INTO @DATA(lv_auart)
    WHERE aufnr = @v_ordem.

    IF sy-subrc <> 0.
      CLEAR lv_auart.
    ENDIF.

    IF lv_auart = 'HR00'.
      DELETE g_codegrptab WHERE codegruppe <> 'F050'. "F050 --> Improdutivo
    ELSE.
      DELETE g_codegrptab WHERE codegruppe = 'F050'.
    ENDIF.

  ENDIF.
  "FF - Ajuste catálogo "A" #134401 - fim


  CLEAR: iqpk1cd, iqpk1cd[].

  "Não foi utilizado matchcode, valor foi digitado na tela.
  IF p_cat = 'OTKAT'. "Parte Obejto
    IF zepm_aponta_cat_notas-oteil IS NOT INITIAL.
      h_code = zepm_aponta_cat_notas-oteil.
      cdgrp  = g_codegrptab-codegruppe.
      lretn  = abap_true.
    ENDIF.

  ELSEIF p_cat = 'FEKAT'. "Sintoma Dano
    IF zepm_aponta_cat_notas-fecod IS NOT INITIAL.
      h_code = zepm_aponta_cat_notas-fecod.
      cdgrp  = g_codegrptab-codegruppe.
      lretn  = abap_true.
    ENDIF.

  ELSEIF p_cat = 'URKAT'. "Causa
    IF zepm_aponta_cat_notas-urcod IS NOT INITIAL.
      h_code = zepm_aponta_cat_notas-urcod.
      cdgrp  = g_codegrptab-codegruppe.
      lretn  = abap_true.
    ENDIF.

  ELSEIF p_cat = 'MFKAT'. "Códigos de ação
    IF zaponta-mncod IS NOT INITIAL.
      h_code = zaponta-mncod.
      cdgrp  = g_codegrptab-codegruppe.
      lretn  = abap_true.
    ENDIF.
  ENDIF.

  "Popup do matchcode com ALV tree
  CALL FUNCTION 'QPK1_GP_CODE_SELECTION'
    EXPORTING
      i_katalogart           = lkatart
      i_codegruppe           = cdgrp
      i_code                 = h_code
      i_sprache              = sy-langu
      i_pickup_mode          = lpick
      i_display_mode         = ldisp
      i_return_if_one        = lretn
    TABLES
      t_qpk1cdtab            = iqpk1cd
      t_codegrptab           = g_codegrptab
    EXCEPTIONS
      no_match_in_range      = 01
      no_user_selection      = 02
      no_authorization       = 03
      no_selection_specified = 04
      object_locked          = 05
      lock_error             = 06
      object_missing         = 07
      code_deactivated       = 08.

  READ TABLE iqpk1cd INDEX 1 INTO DATA(ls_iqpk1cd).

  CASE  p_cat.
    WHEN 'OTKAT'. "Parte Obejto.
      zepm_aponta_cat_notas-otgrp   = ls_iqpk1cd-codegruppe.
      zepm_aponta_cat_notas-oteil   = ls_iqpk1cd-code.
      zepm_aponta_cat_notas-txtcdot = ls_iqpk1cd-kurztextcd.
    WHEN 'FEKAT'. "Sintoma Dano.
      zepm_aponta_cat_notas-fegrp   = ls_iqpk1cd-codegruppe.
      zepm_aponta_cat_notas-fecod   = ls_iqpk1cd-code.
      zepm_aponta_cat_notas-txtcdfe = ls_iqpk1cd-kurztextcd.
    WHEN 'URKAT'. "Causa
      zepm_aponta_cat_notas-urgrp   = ls_iqpk1cd-codegruppe.
      zepm_aponta_cat_notas-urcod   = ls_iqpk1cd-code.
      zepm_aponta_cat_notas-txtcdur = ls_iqpk1cd-kurztextcd.
    WHEN 'MFKAT'. "Códigos de ação
      zaponta-mngrp   = ls_iqpk1cd-codegruppe.
      zaponta-mncod   = ls_iqpk1cd-code.
      zaponta-txtcdma = ls_iqpk1cd-kurztextcd.
    WHEN OTHERS.
  ENDCASE.

  CLEAR ls_iqpk1cd.


  IF sy-dynnr = '0201'.
*    DATA(fieldname) = 'ZAPONTA-MNCOD'.  "Nome do campo onde você deseja definir o cursor
*    SET CURSOR FIELD fieldname .
*
*    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
*      EXPORTING
*        functioncode           = '=ENTER' "ENTER
*      EXCEPTIONS
*        function_not_supported = 1
*        OTHERS                 = 2.
  ELSE.
    CALL SCREEN 100.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BLOQUEAR_CAMPOS_NOTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bloquear_campos_nota .

  LOOP AT SCREEN.
    CASE  screen-group1.
      WHEN 'NOT'. "Notas
        screen-input = 0. "fechar campos para edição
        MODIFY SCREEN.
    ENDCASE.
  ENDLOOP.

  "Desabilitar a edição do texto longo.
  CALL METHOD g_editor->set_readonly_mode
    EXPORTING
      readonly_mode = c_true.

ENDFORM.

FORM desbloquear_campos_nota .

  LOOP AT SCREEN.
    CASE  screen-group1.
      WHEN 'NOT'. "Notas
        screen-input = 1. "abrir campos para edição
        MODIFY SCREEN.
    ENDCASE.
  ENDLOOP.

  "Habilitar a edição do texto longo.
  CALL METHOD g_editor->set_readonly_mode
    EXPORTING
      readonly_mode = c_false.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GUARDA_DADOS_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_guarda_dados_tela TABLES p_dados_tela STRUCTURE zepm_aponta_cat_notas.

  "Lendo dados do texto longo para salvar ao paginar pelas notificações.
  IF v_nota IS NOT INITIAL.
    CALL METHOD g_editor->get_text_as_r3table
      IMPORTING
        table = g_mytable.

    CALL METHOD cl_gui_cfw=>flush
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc NE 0.
*       add your handling
    ENDIF.

*&------------------------------------------------------AOENNING
    CALL METHOD g_editor->get_text_as_stream( IMPORTING text = it_editor ).
*&------------------------------------------------------AOENNING

    CLEAR wa_texto.
    READ TABLE gt_texto WITH KEY qmnum = v_nota INTO wa_texto.
    IF sy-subrc = 0 AND g_mytable[] IS NOT INITIAL.
      "Deletando para inserir o que foi lido da tela g_mytable, pois se o usuário tiver digitado algo novo, nesta tabela terá os dados atuais.
      DELETE gt_texto WHERE qmnum = v_nota.
    ENDIF.
    wa_texto-qmnum = v_nota.
    LOOP AT g_mytable INTO DATA(wa).
      wa_texto-texto = wa.
      APPEND wa_texto TO gt_texto.
    ENDLOOP.

    CALL METHOD g_editor->delete_text. "Limpa o texto longo.

*******************************************************************************
    READ TABLE p_dados_tela WITH KEY notifno = zepm_aponta_cat_notas-notifno TRANSPORTING NO FIELDS.

    IF sy-subrc = 0.
      MODIFY p_dados_tela FROM zepm_aponta_cat_notas TRANSPORTING notifno otgrp oteil txtcdot fegrp fecod
                                                                   txtcdfe fetxt urgrp urcod txtcdur urtxt
                                                                   mngrp mncod txtcdma matxt WHERE notifno = zepm_aponta_cat_notas-notifno.
    ELSE.
      IF zepm_aponta_cat_notas IS NOT INITIAL.
        APPEND zepm_aponta_cat_notas TO p_dados_tela.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_LONG_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_create_long_text .

  IF g_editor IS INITIAL.

*   create control container
    CREATE OBJECT g_editor_container
      EXPORTING
        container_name              = 'TEXTEDITOR1'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.
    IF sy-subrc NE 0.

    ENDIF.
    g_mycontainer = 'TEXTEDITOR1'.

*   create calls constructor, which initializes, creats and links
*   TextEdit Control
    CREATE OBJECT g_editor
      EXPORTING
        parent                     = g_editor_container
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_position          = line_length
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true.
*cl_gui_textedit=>wordwrap_off
*cl_gui_textedit=>WORDWRAP_AT_WINDOWBORDER


*   to handle different containers
    g_container_linked = 1.

    REFRESH g_mytable.

    CALL METHOD g_editor->set_toolbar_mode "Desabilita o tool bar
      EXPORTING
        toolbar_mode = cl_gui_textedit=>false.

    CALL METHOD g_editor->set_statusbar_mode "Desabilita indicador de quantidade de linhas
      EXPORTING
        statusbar_mode = cl_gui_textedit=>false.

  ENDIF.
**  Begin of   "FF #96115
  IF v_ordem IS NOT INITIAL.

    DATA(lv_hr00) = 'HR00'. "Ordem para apontamento de horas improdutivas

    SELECT SINGLE auart
      FROM caufv
      INTO @DATA(dummy)
      WHERE aufnr = @v_ordem
        AND auart = @lv_hr00.
    IF sy-subrc = 0.

      CALL METHOD g_editor->set_visible "Desabilita o control
        EXPORTING
          visible = ''.

    ELSE.

      CALL METHOD g_editor->set_visible "Habilita o control
        EXPORTING
          visible = 'X'.

    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SALVA_TEXTO_LONGO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_salva_texto_longo .
  DATA: tl_lines  TYPE TABLE OF tline,
        st_header TYPE thead,
        st_lines  TYPE tline.

*   retrieve table from control
  CALL METHOD g_editor->get_text_as_r3table
    IMPORTING
      table = g_mytable.
*     if you would like to work with the table contents
*     perform a explicit flush here allthough the method
*     flushes internally (at least up to release 4.6D).
*     The reason: don't rely on internal flushes of control
*     wrappers. These might vanish in the future leading to a
*     malfunction of your transaction. The additional flush here
*     does no harm. The autmation queue is empty and NO additional
*     roundtrip to the frontend will be triggered.
  CALL METHOD cl_gui_cfw=>flush
    EXCEPTIONS
      OTHERS = 1.
  IF sy-subrc NE 0.
*       add your handling
  ENDIF.


  st_header-tdobject = 'QMEL'.
*  st_header-tdname   = v_deliv_numb.
  st_header-tdid     = 'LTXT'.
  st_header-tdspras  = 'P'.

  LOOP AT g_mytable INTO DATA(wa_texto_longo).
    APPEND INITIAL LINE TO tl_lines ASSIGNING FIELD-SYMBOL(<fs_lines>).
    <fs_lines>-tdline   = wa_texto_longo.
    <fs_lines>-tdformat = '*'.
  ENDLOOP.

  CALL FUNCTION 'SAVE_TEXT'
    EXPORTING
      header          = st_header
      savemode_direct = 'X'
    TABLES
      lines           = tl_lines
    EXCEPTIONS
      id              = 1
      language        = 2
      name            = 3
      object          = 4
      OTHERS          = 5.
  .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SET_LONG_TEXT
*&---------------------------------------------------------------------*
FORM f_set_long_text .
  "Setando o texto longo para a respectiva notificação.
  CLEAR g_mytable[].
  LOOP AT  gt_texto INTO DATA(wa_texto) WHERE qmnum = v_nota.
    APPEND INITIAL LINE TO g_mytable ASSIGNING FIELD-SYMBOL(<fs>).
    <fs> = wa_texto-texto.
  ENDLOOP.

  IF g_mytable[] IS NOT INITIAL.
    CALL METHOD g_editor->set_text_as_r3table
      EXPORTING
        table = g_mytable.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_LE_TEXTO_LONGO_NOTIF
*&---------------------------------------------------------------------*
FORM f_le_texto_longo_notif USING p_nota p_only_read.

  DATA: lv_name   TYPE thead-tdname,
        lv_id     TYPE thead-tdid VALUE 'LTXT',
        lv_object TYPE thead-tdobject VALUE 'QMEL'.

*  DATA lt_lines TYPE TABLE OF tline.

  CLEAR: g_mytable[], gt_lines[].

  CHECK v_nota IS NOT INITIAL.

  lv_name = p_nota.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = lv_id
      language                = sy-langu
      name                    = lv_name
      object                  = lv_object
    TABLES
      lines                   = gt_lines
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.
    CLEAR gt_lines.

  ELSE.
    IF p_only_read IS INITIAL.
      "Setando o texto longo para a respectiva notificação.
      LOOP AT  gt_lines INTO DATA(wa_lines).
        APPEND INITIAL LINE TO g_mytable ASSIGNING FIELD-SYMBOL(<fs>).
        <fs> = wa_lines-tdline.
*        <fs> = wa_lines-tdline+1.
      ENDLOOP.
      APPEND INITIAL LINE TO g_mytable."Linha em branco para ficar editável no final do box de observação.

      IF g_mytable[] IS NOT INITIAL.
        CALL METHOD g_editor->set_text_as_r3table
          EXPORTING
            table = g_mytable.

        DATA(lv_tam) = lines( g_mytable ) - 1.

        CALL METHOD g_editor->protect_lines "Protege as linhas que já existiam
          EXPORTING
            from_line    = 1
            protect_mode = 1 "protection mode; eq 0: OFF ; ne 0: ON
            to_line      = lv_tam.

        DATA(lv_ini) = lv_tam + 1.

        CALL METHOD g_editor->protect_lines "Protege as linhas que já existiam
          EXPORTING
            from_line    = lv_ini
            protect_mode = 0. "protection mode; eq 0: OFF ; ne 0: ON

      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  UNLOCK_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM unlock_order .

  DATA aufnr_imp  LIKE  caufvd-aufnr.

  aufnr_imp = v_ordem.

  CALL FUNCTION 'CO_RU_ORDER_DEQUEUE'
    EXPORTING
      aufnr_imp = aufnr_imp.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LIMPAR_HORA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpar_campos .
  CHECK vg_erro IS INITIAL.
  CLEAR: zaponta-isdz, "Hora inicio
         zaponta-iedz, "Hora fim
         zaponta-mncod, "Ação
         zaponta-txtcdma, "Descrição
         zaponta-afrud. "Trabalho real

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_CAMPOS_VAZIOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verifica_campos_vazios CHANGING p_tipo_ordem.
  CLEAR vg_erro.
  DATA(lv_hr00) = 'HR00'. "Ordem para apontamento de horas improdutivas

  CHECK v_nota IS NOT INITIAL. "Só validar para os casos que possuem nota associada.

  SELECT SINGLE auart
    FROM caufv
    INTO p_tipo_ordem
    WHERE aufnr = v_ordem
      AND auart = lv_hr00.

  CHECK sy-subrc <> 0. "Só validar os campos abaixo em caso de ordems <> HR00

  IF zepm_aponta_cat_notas-oteil IS INITIAL OR
     zepm_aponta_cat_notas-fecod IS INITIAL OR
     zepm_aponta_cat_notas-urcod IS INITIAL.

    vg_erro = 'X'.
    MESSAGE s001(eideutilmd) DISPLAY LIKE 'E'. "Não estão preenchidos todos os campos obrigatórios
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHAMAR_ZPM0003
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chamar_zpm0003 .

  DATA: lv_equnr TYPE equnr.

  lv_equnr = obj_main->header-equipment.

  DATA: it_bdcdata    TYPE TABLE OF bdcdata,
        wa_it_bdcdata LIKE LINE OF it_bdcdata.

  DATA opt TYPE ctu_params.

  CLEAR wa_it_bdcdata.
  wa_it_bdcdata-program  = 'ZFTPM_TRFILOL'.
  wa_it_bdcdata-dynpro   = '1000'.
  wa_it_bdcdata-dynbegin = 'X'.
  APPEND wa_it_bdcdata TO it_bdcdata.

  CLEAR wa_it_bdcdata.
  wa_it_bdcdata-fnam = 'BDC_CURSOR'.
  wa_it_bdcdata-fval = 'V_EQUIP'.
  APPEND wa_it_bdcdata TO it_bdcdata.

  CLEAR wa_it_bdcdata.
  wa_it_bdcdata-fnam = 'BDC_OKCODE'.
  wa_it_bdcdata-fval = '=ENTER'.
  APPEND wa_it_bdcdata TO it_bdcdata.

  CLEAR wa_it_bdcdata.
  wa_it_bdcdata-fnam = 'V_EQUIP'.
  wa_it_bdcdata-fval = |{ lv_equnr ALPHA = OUT }|.
  APPEND wa_it_bdcdata TO it_bdcdata.

  opt-dismode = 'E'.

  CALL TRANSACTION 'ZPM0003' USING it_bdcdata OPTIONS FROM opt.

ENDFORM.
