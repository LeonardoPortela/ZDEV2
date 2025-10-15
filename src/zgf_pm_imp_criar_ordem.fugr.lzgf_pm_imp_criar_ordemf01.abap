*----------------------------------------------------------------------*
***INCLUDE LZGF_PM_IMP_CRIAR_ORDEMF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_APONTAMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_APONTAMENTOS  text
*----------------------------------------------------------------------*
FORM f_valida_apontamentos  USING p_t_apontamentos TYPE zpmt0015_t
                                  p_ordens         TYPE ztpm_d_m_ordem_t
                         CHANGING c_msg TYPE char255.


  DATA: zretun    TYPE p DECIMALS 2,
        v_ponta   TYPE p DECIMALS 2,
        cont_erro TYPE p DECIMALS 2,
        lv_aufnr  TYPE aufk-aufnr.
*        wa_apont  TYPE ty_zaponta.

  IF p_t_apontamentos IS NOT INITIAL.

    DATA(lt_apont) = p_t_apontamentos.
    SORT lt_apont BY pernr.
    DELETE ADJACENT DUPLICATES FROM lt_apont COMPARING pernr.

    DELETE lt_apont WHERE pernr IS INITIAL.
    IF sy-subrc IS INITIAL.

      MESSAGE ID 'ZPMMSG' TYPE 'E' NUMBER 000 WITH 'Existem apontamentos sem Nº Pessoal ' 'favor corrigir'  INTO c_msg.
      EXIT.
    ENDIF.
    READ TABLE p_ordens ASSIGNING FIELD-SYMBOL(<fs_ordens>) INDEX 1.
    IF sy-subrc IS INITIAL.

      lv_aufnr = <fs_ordens>-aufnr.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_aufnr
        IMPORTING
          output = lv_aufnr.

      DATA(lt_apont_aux) = lt_apont.
      DELETE lt_apont_aux WHERE vornr IS INITIAL.

      IF lt_apont_aux IS NOT INITIAL.

        SELECT *
          FROM afru
          INTO TABLE @DATA(lt_afru)
          FOR ALL ENTRIES IN @lt_apont_aux
          WHERE aufnr = @lv_aufnr
            AND vornr = @lt_apont_aux-vornr.
        IF sy-subrc IS INITIAL.

          SORT lt_afru BY aueru.
          READ TABLE lt_afru TRANSPORTING  NO FIELDS
          WITH KEY aueru = 'X'
          BINARY SEARCH.
          IF sy-subrc IS INITIAL.

            MESSAGE ID 'ZPMMSG' TYPE 'E' NUMBER 000 WITH 'Não é possível realizar apontamento ' 'ordem com confirmação final!'  INTO c_msg.
            EXIT.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.
*    FREE t_afru[].
*    FREE tl_afru[].
*    Check se ja existe lançamento para empregado no mesmo periodo para mesma ordem ou ordens diferente.
    SELECT *
    FROM afru
    INTO TABLE @DATA(t_afru)
      FOR ALL ENTRIES IN @lt_apont
    WHERE pernr EQ @lt_apont-pernr
      AND stokz NE @abap_true
      AND stzhl EQ ' '.

    SORT t_afru ASCENDING BY pernr.
    CLEAR zretun.
    CLEAR v_ponta.


    LOOP AT p_t_apontamentos ASSIGNING FIELD-SYMBOL(<fs_apontamentos>).

      READ TABLE t_afru ASSIGNING FIELD-SYMBOL(<fs_afru>)
      WITH KEY pernr = <fs_apontamentos>-pernr
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        LOOP AT t_afru ASSIGNING <fs_afru> FROM sy-tabix.

          IF <fs_afru>-pernr <> <fs_apontamentos>-pernr.
            EXIT.
          ENDIF.

          IF ( <fs_afru>-isdd = <fs_apontamentos>-isdd OR <fs_afru>-iedd = <fs_apontamentos>-iedd ) AND <fs_apontamentos>-stokz IS INITIAL.

            IF  <fs_apontamentos>-isdd BETWEEN <fs_afru>-isdd AND <fs_afru>-iedd.

              IF <fs_apontamentos>-isdz BETWEEN <fs_afru>-isdz AND <fs_afru>-iedz.

                ADD 1 TO zretun.

                EXIT.
*
*                MOVE-CORRESPONDING t_afru TO tl_afru.
*                APPEND tl_afru.
              ENDIF.

            ENDIF.

          ENDIF.

*          CLEAR t_afru.

        ENDLOOP.

        IF zretun <> 0.
          MESSAGE ID 'ZPMMSG' TYPE 'E' NUMBER 009 WITH <fs_apontamentos>-pernr  INTO c_msg.
          CONCATENATE c_msg <fs_apontamentos>-pernr INTO c_msg SEPARATED BY space.
          EXIT.
        ENDIF.

      ENDIF.

    ENDLOOP.


*    ELSE.
*
*      DATA(lt_afru) = t_afru.
*      SORT lt_afru BY pernr isdd.
*      DELETE ADJACENT DUPLICATES FROM lt_afru COMPARING pernr isdd.
*
*      LOOP AT p_t_apontamentos ASSIGNING <fs_apontamentos>.
*
*        READ TABLE lt_afru ASSIGNING <w_afru>
*        WITH KEY pernr = <fs_apontamento>-pernr
*                 isdd  = <fs_apontamento>-isdd
*        BINARY SEARCH.
*        IF sy-subrc IS INITIAL.
*          LOOP AT lt_afru  ASSIGNING FIELD-SYMBOL(<w_afru>) FROM sy-tabix.
*
*            IF <w_afru>-pernr <> <fs_apontamento>-pernr OR
*               <w_afru>-isdd  <> <fs_apontamento>-isdd.
*              EXIT.
*            ENDIF.
*
*            IF <w_afru>-isdd = <fs_apontamento>-isdd AND <w_afru>-pernr IS NOT INITIAL.
*              IF <w_afru>-ismne = 'MIN'.
*                <w_afru>-ismnw = ( <w_afru>-ismnw / 60 ).
*                ADD <w_afru>-ismnw TO v_ponta.
*              ELSE.
*
*                ADD <w_afru>-ismnw TO v_ponta.
*              ENDIF.
*            ENDIF.
*          ENDLOOP.
*        ENDIF.
*      ENDLOOP.
*
*

  ENDIF.

*    LOOP AT it_aponta INTO wa_apont WHERE pernr = zaponta-pernr
*                                     AND   isdd = zaponta-isdd OR iedd = zaponta-iedd.
*
*      IF zaponta-pernr = wa_apont-pernr AND zaponta-isdd BETWEEN wa_apont-isdd AND wa_apont-iedd.
*        IF zaponta-pernr = wa_apont-pernr AND zaponta-isdz BETWEEN wa_apont-isdz AND wa_apont-iedz.
*          ADD 1 TO zretun.
*        ENDIF.
*
*      ENDIF.
*      CLEAR wa_apont.
*    ENDLOOP.

*    LOOP AT it_aponta ASSIGNING FIELD-SYMBOL(<l_apont>) WHERE aufnr = zaponta-aufnr
*                                                       AND activity = zaponta-activity.
*      IF <l_apont>-fin_conf IS NOT INITIAL.
*        MESSAGE i000 WITH 'Confimarção final ja foi selecionada'DISPLAY LIKE 'E'.
*        ADD 1 TO cont_erro.
*        CONTINUE.
*      ENDIF.
*    ENDLOOP.


*    IF zretun = 0.
*      Check calculo total de horas digitado.



*      LOOP AT it_aponta ASSIGNING FIELD-SYMBOL(<w_apont>) WHERE pernr = zaponta-pernr
*                                                            AND isdd  = zaponta-isdd.
*
*        IF <w_apont>-isdd  = zaponta-isdd AND <w_apont>-pernr IS NOT INITIAL.
*          IF <w_apont>-duration_normal_unit = 'MIN'.
*            <w_apont>-afrud = ( <w_apont>-afrud / 60 ).
*
*            ADD <w_apont>-afrud TO v_ponta.
*
*          ELSE.
*            ADD <w_apont>-afrud TO v_ponta.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.

*      IF ZAPONTA-DURATION_NORMAL_UNIT = 'MIN'.
*        ZAPONTA-AFRUD = ( ZAPONTA-AFRUD / 60 ).
*      ENDIF.
*
*      v_ponta = ( v_ponta + zaponta-afrud ).
*
*      IF v_ponta > zaponta-einzh.
*        MESSAGE i008 WITH v_ponta zaponta-einzh DISPLAY LIKE 'E'.
*      ELSE.
*        IF cont_erro IS INITIAL.
**          APPEND ZAPONTA TO IT_APONTA.
**          MOVE-CORRESPONDING IT_APONTA TO OBJ_MAIN->IT_OPERA.
**          SORT IT_APONTA ASCENDING BY PERNR ISDD ISDZ.
**          SORT OBJ_MAIN->IT_OPERA ASCENDING BY PERNR ISDD ISDZ.
**          CLEAR CONT_ERRO.
*        ELSE.
**          CLEAR CONT_ERRO.
*        ENDIF.
*      ENDIF.
*    ELSE.
*      MESSAGE i009 WITH zaponta-pernr zaponta-sname DISPLAY LIKE 'E'.
*      IF tl_afru[] IS NOT INITIAL.
**        CALL SCREEN 0400 STARTING AT 5 5 ENDING AT 110 20.
*      ENDIF.
*    ENDIF.
*  ENDIF.
ENDFORM.
