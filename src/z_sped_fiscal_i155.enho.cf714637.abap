"Name: \PR:J_1BECD_MAIN\FO:FORMAT_RECORD_TO_LINE\SE:END\EI
ENHANCEMENT 0 Z_SPED_FISCAL_I155.
*   TYPES:  BEGIN OF ty_bkpf,
*       bukrs TYPE bkpf-bukrs,
*       belnr TYPE bkpf-belnr,
*       gjahr TYPE bkpf-gjahr,
*       budat TYPE bkpf-budat,
*     END OF ty_bkpf.
*
*  TYPES: BEGIN OF ty_bseg,
*         bukrs TYPE bseg-bukrs,
*         belnr TYPE bseg-belnr,
*         gjahr TYPE bseg-gjahr,
*         buzei TYPE bseg-buzei,
*         shkzg TYPE bseg-shkzg,
*         DMBE2 TYPE bseg-dmbtr,
*         hkont TYPE bseg-hkont,
*         dmbtr TYPE bseg-dmbtr,
*       END OF ty_bseg.
*
*  DATA: t_bkpf TYPE STANDARD TABLE OF ty_bkpf,
*        t_bseg TYPE STANDARD TABLE OF ty_bseg,
*        w_bseg TYPE ty_bseg,
*        w_zsped003   TYPE zsped003,
*        ls_sel_i150  TYPE tp_period_i150,
*        ls_i155      TYPE j_1becd_i155_4_s,
*        WL_SALDO_MI2 TYPE FAGLFLEXT-KSLVT,
*        WL_SALDO_MI3 TYPE FAGLFLEXT-KSLVT.
*
*
*  DATA: IT_CONTAS TYPE ZCT_EMP_CONTAS,
*        WA_CONTAS TYPE ZLC_EMP_CONTAS,
*        IT_SALDO_CONTAS   TYPE TABLE OF ZDE_FI_GL_SALDO_FAGLFLEXT WITH HEADER LINE,
*        IT_SALDO_CONTAS_2 TYPE TABLE OF ZDE_FI_GL_SALDO_FAGLFLEXT WITH HEADER LINE,
*        IT_SALDO_CONTAS_3 TYPE TABLE OF ZDE_FI_GL_SALDO_FAGLFLEXT WITH HEADER LINE.
*
*  DATA: REFE1 TYPE HSLXX12,
*        VMES  TYPE MONAT,
*        v_dmbtr TYPE bseg-dmbtr,
*        V_POS   TYPE i,
*        V_POS2  TYPE i,
*        V_SIN(1),
*        V_dc(1).
*
*  FIELD-SYMBOLS: <bukrs> TYPE ANY,
*                 <gjahr> TYPE ANY.
*
*
*  ASSIGN: ('(J_1BECD_MAIN)P_BUKRS') TO <bukrs>,
*          ('(J_1BECD_MAIN)P_GJAHR') TO <gjahr>.
*
*
*   SELECT SINGLE *
*     from ZSPED003
*     into w_zsped003
*   where bukrs = <bukrs>.
*
*  CHECK sy-subrc = 0.
*
*  IF cs_result+1(4) = 'I155'.
*    READ TABLE gt_sel_i150 INTO ls_sel_i150 INDEX 1.
*
*    refresh: IT_SALDO_CONTAS,IT_SALDO_CONTAS_2, IT_SALDO_CONTAS_3, IT_CONTAS.
*
*    WA_CONTAS-BUKRS = <bukrs>.
*    WA_CONTAS-SAKNR = cs_result+6(10).
*    APPEND WA_CONTAS TO IT_CONTAS.
*    "
*    CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
*      EXPORTING
*        RYEAR         =  <gjahr>
*        CONTAS        = IT_CONTAS
*        P_GERAR_TODAS = 'X'
*      TABLES
*        IT_SALDOS     = IT_SALDO_CONTAS
*        IT_SALDOS_2   = IT_SALDO_CONTAS_2
*        IT_SALDOS_3   = IT_SALDO_CONTAS_3
*      EXCEPTIONS
*        MOEDA_NAO_ADM = 1
*        ERRO_LEDGER   = 2
*        OTHERS        = 3.
*
*     IF SY-SUBRC <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*     ENDIF.
*
*     VMES = ls_sel_i150-dt_ini+4(2).
*     SUBTRACT 1 from vmes.
*
*     WL_SALDO_MI2 = 0.
*     READ TABLE IT_SALDO_CONTAS_2 WITH KEY   RACCT  = cs_result+6(10).
*     IF SY-SUBRC IS INITIAL.
*        ADD IT_SALDO_CONTAS_2-SLVT TO WL_SALDO_MI2.
*        DO VMES TIMES VARYING REFE1 FROM IT_SALDO_CONTAS_2-SL01 NEXT IT_SALDO_CONTAS_2-SL02.
*          ADD REFE1 TO WL_SALDO_MI2.
*        ENDDO.
*     ENDIF.
*
*     SELECT bukrs belnr gjahr budat FROM bkpf
*      INTO TABLE t_bkpf
*      WHERE bukrs = <bukrs> AND
*            gjahr = <gjahr> AND
*            budat BETWEEN ls_sel_i150-dt_ini  AND ls_sel_i150-dt_fin AND
*            bstat = space.
*
*     refresh t_bseg.
*     if t_bkpf[] is not INITIAL.
*       SELECT bukrs belnr gjahr buzei shkzg DMBE2 hkont
*       FROM bseg INTO TABLE t_bseg
*       FOR ALL ENTRIES IN t_bkpf
*       WHERE bseg~bukrs = t_bkpf-bukrs AND
*             bseg~belnr = t_bkpf-belnr AND
*             bseg~gjahr = t_bkpf-gjahr AND
*             bseg~hkont = cs_result+6(10).
*     endif.
*
*     DELETE t_bseg WHERE DMBE2 IS INITIAL.
*     clear ls_i155.
*     LOOP AT t_bseg INTO w_bseg.
*       IF w_bseg-shkzg = 'H'.
*         ls_i155-vl_cred = ls_i155-vl_cred + w_bseg-DMBE2.
*       ELSEIF w_bseg-shkzg = 'S'.
*         ls_i155-vl_deb = ls_i155-vl_deb + w_bseg-DMBE2.
*       ENDIF.
*     ENDLOOP.
*
*      "10
*      WL_SALDO_MI3 = WL_SALDO_MI2.
*      if WL_SALDO_MI3 lt 0.
*         MULTIPLY WL_SALDO_MI3 by -1.
*      endif.
*      lv_field = WL_SALDO_MI3.
*      REPLACE '.' WITH ',' INTO lv_field.
*      CONDENSE lv_field NO-GAPS.
*      CONCATENATE cs_result lv_field const_separator  INTO cs_result.
*
*      "11
*      if WL_SALDO_MI2 le 0.
*         ls_i155-IND_DC_INI = 'C'.
*      Else.
*         ls_i155-IND_DC_INI = 'D'.
*      Endif.
*      CONCATENATE cs_result ls_i155-IND_DC_INI const_separator  INTO cs_result.
*
*      "12
*      lv_field = ls_i155-vl_deb.
*      REPLACE '.' WITH ',' INTO lv_field.
*      CONDENSE lv_field NO-GAPS.
*      CONCATENATE cs_result lv_field const_separator  INTO cs_result.
*
*      "13
*      lv_field = ls_i155-vl_cred.
*      REPLACE '.' WITH ',' INTO lv_field.
*      CONDENSE lv_field NO-GAPS.
*      CONCATENATE cs_result lv_field const_separator  INTO cs_result.
*
*      "14
*      WL_SALDO_MI3 = WL_SALDO_MI2 + ls_i155-vl_deb - ls_i155-vl_cred.
*      WL_SALDO_MI2 = WL_SALDO_MI3.
*      if WL_SALDO_MI3 lt 0.
*         MULTIPLY WL_SALDO_MI3 by -1.
*      endif.
*      lv_field = WL_SALDO_MI3.
*      REPLACE '.' WITH ',' INTO lv_field.
*      CONDENSE lv_field NO-GAPS.
*      CONCATENATE cs_result lv_field const_separator  INTO cs_result.
*
*      "15
*      if WL_SALDO_MI2 le 0.
*         ls_i155-IND_DC_INI = 'C'.
*      Else.
*         ls_i155-IND_DC_INI = 'D'.
*      Endif.
*      CONCATENATE cs_result ls_i155-IND_DC_INI const_separator  INTO cs_result.
*
*    ELSEIF cs_result+1(4) = 'I200'.
*      SELECT bukrs belnr gjahr buzei shkzg DMBE2 hkont
*       FROM bseg
*      INTO TABLE t_bseg
*      WHERE bukrs = <bukrs>
*      and   belnr = cs_result+6(10)
*      and   gjahr = <gjahr>.
*      clear ls_i155.
*     LOOP AT t_bseg INTO w_bseg.
*       IF w_bseg-shkzg = 'H'.
*         ls_i155-vl_cred = ls_i155-vl_cred + w_bseg-DMBE2.
*       ENDIF.
*     ENDLOOP.
*    lv_field = ls_i155-vl_cred.
*    REPLACE '.' WITH ',' INTO lv_field.
*    CONDENSE lv_field NO-GAPS.
*    CONCATENATE cs_result lv_field const_separator  INTO cs_result.
*
*    ELSEIF cs_result+1(4) = 'I250'.
*     SELECT bukrs belnr gjahr buzei shkzg DMBE2 hkont dmbtr
*       FROM bseg
*      INTO TABLE t_bseg
*      WHERE bukrs = GS_BKPF-BUKRS
*      and   belnr = GS_BKPF-BELNR
*      and   gjahr = GS_BKPF-GJAHR.
*     clear ls_i155.
*     find FIRST OCCURRENCE OF '|D|' in cs_result match OFFSET V_POS.
*     IF V_POS = 0.
*        find FIRST OCCURRENCE OF '|C|' in cs_result match OFFSET V_POS.
*     ENDIF.
*     clear: v_dmbtr,  V_SIN.
*
*     V_POS2  = v_POS - 18.
*     lv_field = cs_result+18(v_POS2).
*     REPLACE ',' WITH '.' INTO lv_field.
*     CONDENSE lv_field NO-GAPS.
*     v_dmbtr = lv_field.
*
*     if cs_result+v_POS(3) = '|D|'.
*        V_SIN = 'S'.
*        V_dc = 'D'.
*     elseif cs_result+v_POS(3) = '|C|'.
*         V_SIN = 'H'.
*         V_dc = 'C'.
*     endif.
*     LOOP AT t_bseg INTO w_bseg.
*       if w_bseg-hkont = cs_result+6(10) and
*          w_bseg-shkzg = V_SIN and
*          w_bseg-dmbtr = v_dmbtr.
*         ls_i155-vl_cred = ls_i155-vl_cred + w_bseg-DMBE2.
*       ENDIF.
*     ENDLOOP.
*    lv_field = ls_i155-vl_cred.
*    REPLACE '.' WITH ',' INTO lv_field.
*    CONDENSE lv_field NO-GAPS.
*    CONCATENATE cs_result lv_field const_separator V_dc const_separator INTO cs_result.
*
*    ELSEIF cs_result+1(4) = 'I355'.
*       refresh: IT_SALDO_CONTAS,IT_SALDO_CONTAS_2, IT_SALDO_CONTAS_3, IT_CONTAS.
*
*       WA_CONTAS-BUKRS = <bukrs>.
*       WA_CONTAS-SAKNR = cs_result+6(10).
*       APPEND WA_CONTAS TO IT_CONTAS.
*       "
*       CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
*         EXPORTING
*           RYEAR         =  <gjahr>
*           CONTAS        = IT_CONTAS
*           P_GERAR_TODAS = 'X'
*         TABLES
*           IT_SALDOS     = IT_SALDO_CONTAS
*           IT_SALDOS_2   = IT_SALDO_CONTAS_2
*           IT_SALDOS_3   = IT_SALDO_CONTAS_3
*         EXCEPTIONS
*           MOEDA_NAO_ADM = 1
*           ERRO_LEDGER   = 2
*           OTHERS        = 3.
*
*        IF SY-SUBRC <> 0.
*          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*        ENDIF.
*
*        WL_SALDO_MI2 = 0.
*        READ TABLE IT_SALDO_CONTAS_2 WITH KEY   RACCT  = cs_result+6(10).
*        IF SY-SUBRC IS INITIAL.
*           ADD IT_SALDO_CONTAS_2-SL16 TO WL_SALDO_MI2.
*        ENDIF.
*        WL_SALDO_MI3 = WL_SALDO_MI2.
*        if WL_SALDO_MI3 lt 0.
*           MULTIPLY WL_SALDO_MI3 by -1.
*        endif.
*        lv_field = WL_SALDO_MI3.
*        REPLACE '.' WITH ',' INTO lv_field.
*        CONDENSE lv_field NO-GAPS.
*        CONCATENATE cs_result lv_field const_separator  INTO cs_result.
*        if WL_SALDO_MI2 le 0.
*         ls_i155-IND_DC_INI = 'D'.
*        Else.
*         ls_i155-IND_DC_INI = 'C'.
*        Endif.
*      CONCATENATE cs_result ls_i155-IND_DC_INI const_separator  INTO cs_result.
*   ENDIF.



ENDENHANCEMENT.
