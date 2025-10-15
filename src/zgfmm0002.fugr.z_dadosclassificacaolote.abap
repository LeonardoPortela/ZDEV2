FUNCTION z_dadosclassificacaolote.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_MATNR STRUCTURE  ZMME_CL
*"      T_ZMMT0027 STRUCTURE  ZMMT0027 OPTIONAL
*"      T_RETURN STRUCTURE  ZMME_CL
*"  EXCEPTIONS
*"      ERRO4
*"----------------------------------------------------------------------
*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Sobral                                             &*
*& Data.....: 13/05/2013                                              &*
*& Descrição: Classificação de lotes                                  &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*&                                                                    &*
*&--------------------------------------------------------------------&*


** CONSTANTS
**----------------------------------------------------------------------
  CONSTANTS: c_023  TYPE c  LENGTH 3 VALUE '023',
             c_mch1 TYPE c  LENGTH 4 VALUE 'MCH1'.



** TYPES
**----------------------------------------------------------------------
  TYPES: BEGIN OF ty_inob,
           matnr TYPE zmme_cl-matnr,
           charg TYPE zmme_cl-charg,
           cuobj TYPE inob-cuobj,
*            CUOBJ_AUX TYPE KSSK-OBJEK,
           objek TYPE kssk-objek,
           atinn TYPE ausp-atinn,
         END OF ty_inob,

         BEGIN OF ty_inob2,
           matnr TYPE zmme_cl-matnr,
           charg TYPE zmme_cl-charg,
*            CUOBJ     TYPE KSSK-OBJEK,
           objek TYPE kssk-objek,
           atinn TYPE ausp-atinn,
         END OF ty_inob2,

         BEGIN OF ty_kssk,
           objek TYPE kssk-objek,
           mafid TYPE kssk-mafid,
           klart TYPE kssk-klart,
           clint TYPE kssk-clint,
           adzhl TYPE kssk-adzhl,
         END OF ty_kssk,

         BEGIN OF ty_ksml,
           clint TYPE ksml-clint,
           posnr TYPE ksml-posnr,
           adzhl TYPE ksml-adzhl,
           imerk TYPE ksml-imerk,
         END OF ty_ksml,

         BEGIN OF ty_ausp,
           objek TYPE ausp-objek,
           atinn TYPE ausp-atinn,
           atzhl TYPE ausp-atzhl,
           mafid TYPE ausp-mafid,
           klart TYPE ausp-klart,
           adzhl TYPE ausp-adzhl,
           atwrt TYPE ausp-atwrt,
         END OF ty_ausp,

         BEGIN OF ty_aux,
*            CUOBJ     TYPE KSSK-OBJEK,
           objek TYPE kssk-objek,
           clint TYPE kssk-clint,
           imerk TYPE ksml-imerk,
         END OF ty_aux.

** INTERNAL TABLES
**----------------------------------------------------------------------
  DATA: it_inob      TYPE TABLE OF ty_inob,
        it_inob_aux  TYPE TABLE OF ty_inob,
        it_inob2     TYPE TABLE OF ty_inob2,
        it_inob2_aux TYPE TABLE OF ty_inob2,
        it_kssk      TYPE TABLE OF ty_kssk,
        it_ksml      TYPE TABLE OF ty_ksml,
        it_ausp      TYPE TABLE OF ty_ausp,
        it_aux       TYPE TABLE OF ty_aux.

** WORK AREAS
**----------------------------------------------------------------------
  DATA: wa_inob     TYPE ty_inob,
        wa_inob_aux TYPE ty_inob,
        wa_inob2    TYPE ty_inob2,
        wa_kssk     TYPE ty_kssk,
        wa_ksml     TYPE ty_ksml,
        wa_ausp     TYPE ty_ausp,
        wa_aux      TYPE ty_aux.

** VARIABLES
**----------------------------------------------------------------------
  DATA: v_objek   TYPE inob-objek,
        v_objek2  TYPE inob-objek,
        l_nomevar TYPE char50.
  DATA: v_len               TYPE i,
        i                   TYPE i,
        v_esp_branco        TYPE i,
        v_espaco_branco(40),
        v_material          TYPE c LENGTH 40,
        v_space             TYPE char22,
        zmatnr_             TYPE char18,
        zmatnr              TYPE matnr.
  FIELD-SYMBOLS:      <fs_valor> TYPE any.
** RANGES
**----------------------------------------------------------------------

**Inicio
  CLEAR: t_return. REFRESH: t_return.

*-CS2022000332-#82292-26.07.2022-JT-inicio
*----------------------------------------------------------
*-trata se adquirico de terceiros
*----------------------------------------------------------
  READ TABLE t_zmmt0027 INTO DATA(w_zmmt0027) INDEX 1.

  IF sy-subrc = 0 AND w_zmmt0027-adquirido_terc = abap_true.
    SELECT *
      FROM zppt0014
      INTO TABLE @DATA(t_zppt0014)
     WHERE clint = 367.

    LOOP AT t_zmmt0027 INTO w_zmmt0027.
      LOOP AT t_zppt0014 INTO DATA(w_zppt0014).

        l_nomevar = 'W_ZMMT0027-' && w_zppt0014-referencia.
        ASSIGN (l_nomevar)  TO <fs_valor>.
        CHECK sy-subrc = 0.

        t_return-matnr = w_zmmt0027-matnr.
        t_return-charg = w_zmmt0027-charg.
        t_return-atinn = w_zppt0014-atinn.
        t_return-atwrt = <fs_valor>.
        APPEND t_return.
      ENDLOOP.
    ENDLOOP.

  ELSE.
*-CS2022000332-#82292-26.07.2022-JT-fim

*** Buscar com ATINN - Inicio
    LOOP AT t_matnr WHERE atinn IS NOT INITIAL.

*** ajuste ZMM0037 #119366 conversão hana - BG INICIO
*      v_len = strlen( t_matnr-matnr ).
*      v_esp_branco = 40 - v_len.
*      WHILE i < v_esp_branco.
*
*        CONCATENATE v_espaco_branco '&' INTO v_espaco_branco.
*        i = i + 1.
*      ENDWHILE.
*      CONCATENATE t_matnr-matnr v_espaco_branco  t_matnr-charg INTO v_objek.
*      TRANSLATE v_objek USING '& '.
      "REPLACE  WITH  into .
      "CONCATENATE t_matnr-matnr v_espaco_branco t_matnr-charg INTO v_objek2.

*      MOVE: v_objek2       TO wa_inob2-objek,
*           t_matnr-atinn TO wa_inob2-atinn,
*           t_matnr-matnr TO wa_inob2-matnr,
*           t_matnr-charg TO wa_inob2-charg.
*      APPEND wa_inob2 TO it_inob2_aux.
*      CLEAR wa_inob2.
**** ajuste ZMM0037 #119366 conversão hana - BG  FIM
*      MOVE: v_objek       TO wa_inob2-objek,
*            t_matnr-atinn TO wa_inob2-atinn,
*            t_matnr-matnr TO wa_inob2-matnr,
*            t_matnr-charg TO wa_inob2-charg.
*      APPEND wa_inob2 TO it_inob2.

      CLEAR: v_objek2, zmatnr_.
      zmatnr_ = |{ t_matnr-matnr ALPHA = IN }|.
      CONCATENATE zmatnr_ v_space t_matnr-charg INTO v_objek2 RESPECTING BLANKS.
      MOVE: v_objek2       TO wa_inob2-objek,
            t_matnr-atinn TO wa_inob2-atinn,
            zmatnr_ TO wa_inob2-matnr,
            t_matnr-charg TO wa_inob2-charg.
      APPEND wa_inob2 TO it_inob2.

      "Setar valores convertido com material 40 caractere.
      CLEAR: zmatnr.
      zmatnr = |{ t_matnr-matnr ALPHA = IN }|.
      CLEAR: v_objek2.
      v_objek2 = |{ t_matnr-matnr }{ t_matnr-charg }|.

      MOVE: v_objek2       TO wa_inob2-objek,
            zmatnr        TO wa_inob2-matnr,
            t_matnr-charg TO wa_inob2-charg.
      APPEND wa_inob2 TO it_inob2.
    ENDLOOP.

    IF it_inob2[] IS NOT INITIAL.

      SELECT cuobj objek FROM inob
        INTO CORRESPONDING FIELDS OF TABLE it_inob
        FOR ALL ENTRIES IN it_inob2
      WHERE objek EQ it_inob2-objek
        AND klart EQ c_023
        AND obtab EQ c_mch1.

      IF it_inob[] IS INITIAL.
        SELECT cuobj objek FROM inob
        INTO CORRESPONDING FIELDS OF TABLE it_inob
        FOR ALL ENTRIES IN it_inob2_aux
        WHERE objek EQ it_inob2_aux-objek
              AND klart EQ c_023
              AND obtab EQ c_mch1.
      ENDIF.


      IF sy-subrc = 0.
        SORT: it_inob2 BY objek matnr charg,
              it_inob  BY objek.

*      LOOP AT IT_INOB INTO WA_INOB.
*        LOOP AT IT_INOB2 INTO WA_INOB2 WHERE OBJEK = WA_INOB-OBJEK.
*          MOVE: WA_INOB-CUOBJ  TO WA_INOB_AUX-OBJEK,
*                WA_INOB2-ATINN TO WA_INOB_AUX-ATINN,
*                WA_INOB2-MATNR TO WA_INOB_AUX-MATNR,
*                WA_INOB2-CHARG TO WA_INOB_AUX-CHARG.
*          APPEND WA_INOB_AUX TO IT_INOB_AUX.
*        ENDLOOP.
*      ENDLOOP.

        LOOP AT it_inob2 INTO wa_inob2.
          READ TABLE it_inob INTO wa_inob WITH KEY objek = wa_inob2-objek BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            MOVE: wa_inob-cuobj  TO wa_inob_aux-objek,
                  wa_inob2-atinn TO wa_inob_aux-atinn,
                  wa_inob2-matnr TO wa_inob_aux-matnr,
                  wa_inob2-charg TO wa_inob_aux-charg.
            APPEND wa_inob_aux TO it_inob_aux.
            CLEAR: wa_inob, wa_inob_aux.
          ENDIF.
        ENDLOOP.

        REFRESH: it_inob.
        it_inob = it_inob_aux.

        SELECT  objek atinn atzhl mafid klart adzhl atwrt
        FROM ausp
          INTO CORRESPONDING FIELDS OF TABLE it_ausp
          FOR ALL ENTRIES IN it_inob
        WHERE objek EQ it_inob-objek
          AND atinn EQ it_inob-atinn
          AND klart EQ c_023.

        IF it_ausp[] IS NOT INITIAL.
          SORT it_ausp  BY objek atinn atzhl mafid klart adzhl atwrt.
          SORT it_inob  BY objek matnr charg.

          LOOP AT it_ausp INTO wa_ausp.
            MOVE-CORRESPONDING wa_ausp TO t_return.

            READ TABLE it_inob INTO wa_inob WITH KEY objek = wa_ausp-objek
                                            BINARY SEARCH.
            MOVE: wa_inob-matnr TO t_return-matnr,
                  wa_inob-charg TO t_return-charg.
            APPEND t_return.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
*** Buscar com ATINN - Fim

*** Buscar ATINN depois localizar ATWRT - Inicio
    CLEAR:    wa_inob, wa_inob2, wa_inob_aux, wa_ausp.
    REFRESH:  it_inob, it_inob2, it_inob_aux, it_ausp.


    "================================Comentado para tese HANA" / AOENNING / Apos analisar os dados na tabela INOB, devera descomentar.
    LOOP AT t_matnr WHERE atinn IS INITIAL.
**** ajuste ZMM0037 #119366 conversão hana - BG INICIO
*      v_len = strlen( t_matnr-matnr ).
*      v_esp_branco = 40 - v_len.
*      WHILE i < v_esp_branco.
*
*        CONCATENATE v_espaco_branco '&' INTO v_espaco_branco.
*        i = i + 1.
*      ENDWHILE.
** ---> S4 Migration - 25/07/2023 - LA
*      CONCATENATE v_material v_espaco_branco  t_matnr-charg INTO v_objek RESPECTING BLANKS.
*      TRANSLATE v_objek USING '& '.
*      CONDENSE v_objek NO-GAPS.
** <--- S4 Migration - 25/07/2023 - LA

      CLEAR: v_objek2, zmatnr_.
      zmatnr_ = |{ t_matnr-matnr ALPHA = IN }|.
      CONCATENATE zmatnr_ v_space t_matnr-charg INTO v_objek2 RESPECTING BLANKS.
      MOVE: v_objek2       TO wa_inob2-objek,
*          T_MATNR-ATINN TO WA_INOB2-ATINN,
            zmatnr_       TO wa_inob2-matnr,
            t_matnr-charg TO wa_inob2-charg.
      APPEND wa_inob2 TO it_inob2.

      "Setar valores convertido com material 40 caractere.
      CLEAR: zmatnr.
      zmatnr = |{ t_matnr-matnr ALPHA = IN }|.
      CLEAR: v_objek2.
      v_objek2 = |{ t_matnr-matnr }{ t_matnr-charg }|.

      MOVE: v_objek2       TO wa_inob2-objek,
            zmatnr        TO wa_inob2-matnr,
            t_matnr-charg TO wa_inob2-charg.
      APPEND wa_inob2 TO it_inob2.

    ENDLOOP.
    "================================Comentado para tese HANA" / AOENNING / Apos analisar os dados na tabela INOB, devera descomentar.

    IF it_inob2[] IS NOT INITIAL.
      SELECT cuobj objek FROM inob
        INTO CORRESPONDING FIELDS OF TABLE it_inob
        FOR ALL ENTRIES IN it_inob2
      WHERE obtab EQ c_mch1
        AND objek EQ it_inob2-objek
        AND klart EQ c_023.

      IF sy-subrc = 0.
        SORT it_inob2 BY objek matnr charg.

        LOOP AT it_inob INTO wa_inob.
          READ TABLE it_inob2 INTO wa_inob2 WITH KEY objek = wa_inob-objek
                                            BINARY SEARCH.

          MOVE: wa_inob-cuobj     TO wa_inob-objek,
*              WA_INOB2-ATINN TO WA_INOB-ATINN,
                wa_inob2-matnr TO wa_inob-matnr,
                wa_inob2-charg TO wa_inob-charg.
          MODIFY it_inob FROM wa_inob.
        ENDLOOP.

        SELECT  objek mafid klart clint adzhl
        FROM kssk
          INTO CORRESPONDING FIELDS OF TABLE it_kssk
          FOR ALL ENTRIES IN it_inob
        WHERE objek EQ it_inob-objek
          AND klart EQ c_023.
        SORT it_kssk BY objek mafid klart clint adzhl.

        IF sy-subrc = 0.
          SELECT  clint posnr adzhl imerk
          FROM ksml
            INTO CORRESPONDING FIELDS OF TABLE it_ksml
            FOR ALL ENTRIES IN it_kssk
          WHERE clint EQ it_kssk-clint
            AND klart EQ c_023.
          SORT it_ksml BY clint posnr adzhl imerk.

          IF sy-subrc = 0.
            LOOP AT it_inob INTO wa_inob.
              READ TABLE it_kssk INTO wa_kssk WITH KEY objek = wa_inob-objek
                                              BINARY SEARCH.

              LOOP AT it_ksml INTO wa_ksml WHERE clint = wa_kssk-clint.
                wa_aux-objek  = wa_inob-objek.
                wa_aux-clint  = wa_kssk-clint.
                wa_aux-imerk  = wa_ksml-imerk.
                COLLECT wa_aux INTO it_aux.
              ENDLOOP.
            ENDLOOP.

            SELECT  objek atinn atzhl mafid klart adzhl atwrt
            FROM ausp
              APPENDING CORRESPONDING FIELDS OF TABLE it_ausp
              FOR ALL ENTRIES IN it_aux
            WHERE objek EQ it_aux-objek
              AND atinn EQ it_aux-imerk
              AND klart EQ c_023.

            IF it_ausp[] IS NOT INITIAL.
              SORT it_ausp  BY objek atinn atzhl mafid klart adzhl atwrt.
              SORT it_inob  BY objek matnr charg.

              LOOP AT it_ausp INTO wa_ausp.
                MOVE-CORRESPONDING wa_ausp TO t_return.

                READ TABLE it_inob INTO wa_inob WITH KEY objek = wa_ausp-objek
                                                BINARY SEARCH.

*                wa_inob-matnr = |{ wa_inob-matnr ALPHA = IN }|.
                MOVE: wa_inob-matnr TO t_return-matnr,
                      wa_inob-charg TO t_return-charg.
                APPEND t_return.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
*** Buscar ATINN depois localizar ATWRT - Fim

  ENDIF. "IF sy-subrc = 0 AND w_zmmt0027-adquirido_terc = abap_true.

  IF t_return[] IS INITIAL.
    RAISE erro4.
  ELSE.
    SORT t_return BY matnr charg atinn.
    DELETE ADJACENT DUPLICATES FROM t_return COMPARING matnr charg atinn.
  ENDIF.

ENDFUNCTION.
