*&---------------------------------------------------------------------*
*&  Include           ZMMR106_CLASS
*&---------------------------------------------------------------------*

CLASS ZCL_RELATORIO_CCTM DEFINITION.

  PUBLIC SECTION.
    DATA: AT_TABLE TYPE TABLE OF TY_MM.

    METHODS:
      "M_SELECTS,
      M_SEL_DADOS_,
      M_SEL_DADOS,
      M_SEL_DADOS_SER,
      M_SEL_DADOS_MAT,
      M_SEL_DADOS_ALL,
      M_CONCATENA,
      M_AGRUPA_DADOS,
      M_AGRUPA,
      M_AGRUPA_,
      M_LAYOUT,
      VERIFICAR_STATUS IMPORTING I_TABLE TYPE ANY TABLE,
      M_ESTORNO,
      SHDB,
      TIME_OUT IMPORTING I_TEXTO TYPE STRING.

ENDCLASS.                    "ZCL_SELECIONA_DADOS DEFINITION

*----------------------------------------------------------------------*
*       CLASS ZCL_ALV_TOOBAR IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ZCL_ALV_TOOLBAR DEFINITION.

  PUBLIC SECTION.
    METHODS: CONSTRUCTOR      IMPORTING IO_ALV_GRID  TYPE REF TO CL_GUI_ALV_GRID,
      ON_TOOLBAR       FOR EVENT TOOLBAR      OF CL_GUI_ALV_GRID IMPORTING E_OBJECT,
      HANDLE_ESTORNO   FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID IMPORTING E_UCOMM,
      ON_DOUBLE_CLICK  FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID IMPORTING E_ROW E_COLUMN,
      ON_CLICK         FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO.

ENDCLASS.                    "ZCL_ALV_TOOBAR IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ZCL_SELECIONA_DADOS IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ZCL_RELATORIO_CCTM IMPLEMENTATION.

  METHOD: M_SEL_DADOS.

    FREE: TW_EKKO, TW_ACEITE, TW_MIGO, TW_MIRO, TW_BK_MIGO,
          TW_BS_MIGO, TW_BK_MIRO, TW_BS_MIRO, TW_MIGO_M, TW_MIRO_M,
          TW_SOFTEXPERT.

    ME->M_SEL_DADOS_( ).
    ME->M_SEL_DADOS_ALL( ).
    ME->M_CONCATENA( ).

  ENDMETHOD.                    "M_SEL_MIRO

  METHOD M_SEL_DADOS_SER.

*  Seleciona a folha de aceite
    SELECT EBELN BEWTP BELNR GJAHR
      FROM  EKBE
        INTO TABLE TW_ACEITE
          FOR ALL ENTRIES IN TW_BK_MIGO
            WHERE BELNR EQ TW_BK_MIGO-BELNR_KEY
              AND GJAHR EQ TW_BK_MIGO-GJAHR_KEY
              AND BEWTP EQ 'D'
              AND DMBTR NE 0.

    CHECK NOT TW_ACEITE[] IS INITIAL.

*   Folha de aceite para busca a MIGO
*   TW_ACEITE -> TW_MIGO
    SELECT EBELN LFBNR LFGJA BEWTP BELNR GJAHR SHKZG
      FROM EKBE
      APPENDING TABLE TW_MIGO
      FOR ALL ENTRIES IN TW_ACEITE
      WHERE EBELN EQ TW_ACEITE-EBELN
        AND LFBNR EQ TW_ACEITE-BELNR
        AND BUDAT IN S_DTP
        AND BEWTP EQ 'E'.

*   Folha de aceite para busca a MIRO
*   TW_ACEITE -> TW_MIRO
    SELECT EBELN LFBNR LFGJA BEWTP BELNR GJAHR SHKZG
      FROM EKBE
      APPENDING TABLE TW_MIRO
      FOR ALL ENTRIES IN TW_ACEITE
      WHERE EBELN EQ TW_ACEITE-EBELN
        AND LFBNR EQ TW_ACEITE-BELNR
        AND BUDAT IN S_DTP
        AND BEWTP EQ 'Q'.

    IF NOT TW_MIRO IS INITIAL.

      FREE: IT_RBKP.

      SELECT * FROM RBKP
        INTO TABLE IT_RBKP
        FOR ALL ENTRIES IN TW_MIRO
        WHERE BELNR EQ TW_MIRO-BELNR.

      LOOP AT IT_RBKP INTO WA_RBKP WHERE NOT STBLG IS INITIAL.
        DELETE TW_MIRO WHERE BELNR EQ WA_RBKP-BELNR AND GJAHR EQ WA_RBKP-GJAHR.
      ENDLOOP.

      SELECT BELNR ID_PROCESS
        FROM ZMMT0094
       APPENDING TABLE TW_SOFTEXPERT
       FOR ALL ENTRIES IN TW_MIRO
       WHERE BELNR EQ TW_MIRO-BELNR.

    ENDIF.


  ENDMETHOD.                    "M_SEL_DADOS_SER
  METHOD M_SEL_DADOS_MAT.

*  Seleciona a folha de aceite
    SELECT EBELN BEWTP BELNR GJAHR
      FROM  EKBE
        INTO TABLE TW_ACEITE
          FOR ALL ENTRIES IN TW_BK_MIGO
            WHERE BELNR EQ TW_BK_MIGO-BELNR_KEY
              AND GJAHR EQ TW_BK_MIGO-GJAHR_KEY
              AND BEWTP EQ 'D'
              AND DMBTR NE 0.

    IF NOT TW_ACEITE IS INITIAL.

*   Folha de aceite para busca a MIGO
*   TW_ACEITE -> TW_MIGO
      SELECT EBELN LFBNR LFGJA BEWTP BELNR GJAHR SHKZG
        FROM EKBE
        APPENDING TABLE TW_MIGO
        FOR ALL ENTRIES IN TW_ACEITE
        WHERE EBELN EQ TW_ACEITE-EBELN
          AND LFBNR EQ TW_ACEITE-BELNR
          AND BUDAT IN S_DTP
          AND BEWTP EQ 'E'.

    ENDIF.


    SELECT EBELN LFBNR LFGJA BEWTP BELNR GJAHR SHKZG
      FROM  EKBE
      APPENDING TABLE TW_MIGO_M
          FOR ALL ENTRIES IN TW_BK_MIGO
            WHERE BELNR EQ TW_BK_MIGO-BELNR_KEY
              AND GJAHR EQ TW_BK_MIGO-GJAHR_KEY
              AND BEWTP EQ 'E'.

    SORT TW_MIGO BY BELNR BUKRS.
    DELETE ADJACENT DUPLICATES FROM TW_MIGO COMPARING BELNR.
    SORT TW_MIGO_M BY BELNR BUKRS.
    DELETE ADJACENT DUPLICATES FROM TW_MIGO_M COMPARING BELNR.

    LOOP AT TW_MIGO INTO WA_MIGO_M.
      DELETE TW_MIGO_M WHERE BELNR EQ WA_MIGO_M-BELNR.
    ENDLOOP.

** INICIO Elimina os Estornos
    ME->VERIFICAR_STATUS( I_TABLE = TW_MIGO_M ).
    MOVE ME->AT_TABLE TO TW_MIGO_M.
** FIM Elimina os Estornos

    FREE:  TW_MIGO.
    MOVE   TW_MIGO_M      TO TW_MIGO.
    MOVE   TW_MIGO        TO TW_MIGO_S_MIRO.
    DELETE TW_MIGO_S_MIRO WHERE LFBNR IS NOT INITIAL.
    DELETE TW_MIGO        WHERE LFBNR IS     INITIAL.

*   Busca as MIRO das MIGO
*   TW_MIGO -> TW_MIRO
    IF NOT TW_MIGO IS INITIAL.

      SELECT EBELN LFBNR LFGJA BEWTP BELNR GJAHR SHKZG
        FROM  EKBE
         INTO TABLE TW_MIRO
            FOR ALL ENTRIES IN TW_BK_MIGO
              WHERE BELNR EQ TW_BK_MIGO-BELNR_KEY
                AND GJAHR EQ TW_BK_MIGO-GJAHR_KEY
                AND BEWTP EQ 'Q'.

      IF NOT TW_MIRO IS INITIAL.

        SELECT BELNR ID_PROCESS
          FROM ZMMT0094
          INTO TABLE TW_SOFTEXPERT
         FOR ALL ENTRIES IN TW_MIRO
         WHERE BELNR EQ TW_MIRO-BELNR.

        FREE: IT_RBKP.

        SELECT * FROM RBKP
          INTO TABLE IT_RBKP
          FOR ALL ENTRIES IN TW_MIRO
          WHERE BELNR EQ TW_MIRO-BELNR.

        LOOP AT IT_RBKP INTO WA_RBKP WHERE NOT STBLG IS INITIAL.
          DELETE TW_MIRO WHERE BELNR EQ WA_RBKP-BELNR
                           AND GJAHR EQ WA_RBKP-GJAHR.
        ENDLOOP.

      ENDIF.

    ENDIF.

  ENDMETHOD.                    "M_SEL_DADOS_MAT

  METHOD M_SEL_DADOS_ALL.

    CHECK NOT TW_BK_MIGO IS INITIAL.

    SELECT EBELN LFBNR LFGJA BEWTP BELNR GJAHR SHKZG XBLNR BUDAT
      FROM  EKBE
      APPENDING TABLE TW_MIGO_M
          FOR ALL ENTRIES IN TW_BK_MIGO
            WHERE BELNR EQ TW_BK_MIGO-BELNR_KEY
              AND GJAHR EQ TW_BK_MIGO-GJAHR_KEY
              AND BEWTP EQ 'E'.

    IF NOT TW_MIGO_M IS INITIAL.

      FREE: IT_MKPF.
      SELECT * FROM MKPF
        APPENDING TABLE IT_MKPF
        FOR ALL ENTRIES IN TW_MIGO_M
        WHERE MBLNR EQ TW_MIGO_M-BELNR.

      SELECT EBELN BEWTP BELNR GJAHR
        FROM  EKBE
          INTO TABLE TW_ACEITE
            FOR ALL ENTRIES IN TW_MIGO_M
              WHERE BELNR EQ TW_MIGO_M-LFBNR
                AND BEWTP EQ 'D'
                AND DMBTR NE 0.
    ENDIF.

    IF NOT TW_ACEITE[] IS INITIAL.
*   Folha de aceite para busca a MIGO
*   TW_ACEITE -> TW_MIGO
      SELECT EBELN LFBNR LFGJA BEWTP BELNR GJAHR SHKZG XBLNR BUDAT
        FROM EKBE
        APPENDING TABLE TW_MIGO
        FOR ALL ENTRIES IN TW_ACEITE
        WHERE EBELN EQ TW_ACEITE-EBELN
          AND LFBNR EQ TW_ACEITE-BELNR
          AND BUDAT IN S_DTP
          AND BEWTP EQ 'E'.

      IF NOT TW_MIGO IS INITIAL.

        FREE: IT_MKPF.
        SELECT * FROM MKPF
          APPENDING TABLE IT_MKPF
          FOR ALL ENTRIES IN TW_MIGO
          WHERE MBLNR EQ TW_MIGO-BELNR.

      ENDIF.

*   Folha de aceite para busca a MIRO
*   TW_ACEITE -> TW_MIRO
      SELECT EBELN LFBNR LFGJA BEWTP BELNR GJAHR SHKZG XBLNR BUDAT
        FROM EKBE
        APPENDING TABLE TW_MIRO
        FOR ALL ENTRIES IN TW_ACEITE
        WHERE EBELN EQ TW_ACEITE-EBELN
          AND LFBNR EQ TW_ACEITE-BELNR
          AND BUDAT IN S_DTP
          AND BEWTP EQ 'Q'.

      IF NOT TW_MIRO IS INITIAL.

        FREE: IT_RBKP.
        SELECT * FROM RBKP
          INTO TABLE IT_RBKP
          FOR ALL ENTRIES IN TW_MIRO
          WHERE BELNR EQ TW_MIRO-BELNR.

        IF IT_RBKP IS NOT INITIAL.
          SELECT BELNR ID_PROCESS
            FROM ZMMT0094
           APPENDING TABLE TW_SOFTEXPERT
           FOR ALL ENTRIES IN IT_RBKP
           WHERE BELNR EQ IT_RBKP-BELNR.
        ENDIF.

        LOOP AT IT_RBKP INTO WA_RBKP WHERE NOT STBLG IS INITIAL.
          READ TABLE TW_MIRO INTO WA_MIRO_M WITH KEY BELNR = WA_RBKP-BELNR
                                                     GJAHR = WA_RBKP-GJAHR.
          IF SY-SUBRC = 0.
            WA_MIRO_M-GSBER = WA_RBKP-GSBER.
            MODIFY TW_MIRO FROM WA_MIRO_M INDEX SY-TABIX TRANSPORTING GSBER.
          ENDIF.
        ENDLOOP.

        LOOP AT IT_RBKP INTO WA_RBKP WHERE NOT STBLG IS INITIAL.
          DELETE TW_MIRO WHERE BELNR EQ WA_RBKP-BELNR
                           AND GJAHR EQ WA_RBKP-GJAHR.
        ENDLOOP.

      ENDIF.

    ENDIF.

    SORT TW_MIGO BY BELNR BUKRS.
    DELETE ADJACENT DUPLICATES FROM TW_MIGO COMPARING BELNR.
    SORT TW_MIGO_M BY BELNR BUKRS.
    DELETE ADJACENT DUPLICATES FROM TW_MIGO_M COMPARING BELNR.

    LOOP AT TW_MIGO INTO WA_MIGO_M.
      DELETE TW_MIGO_M WHERE BELNR EQ WA_MIGO_M-BELNR.
    ENDLOOP.

** INICIO Elimina os Estornos
    ME->VERIFICAR_STATUS( I_TABLE = TW_MIGO_M ).
    MOVE ME->AT_TABLE TO TW_MIGO_M.
** FIM Elimina os Estornos

    LOOP AT TW_MIGO_M INTO WA_MIGO_M.
      APPEND WA_MIGO_M TO TW_MIGO.
    ENDLOOP.

    SELECT EBELN LFBNR LFGJA BEWTP BELNR GJAHR SHKZG XBLNR BUDAT
     FROM  EKBE
       INTO TABLE TW_MIRO
          FOR ALL ENTRIES IN TW_BK_MIGO
            WHERE BELNR EQ TW_BK_MIGO-BELNR_KEY
              AND GJAHR EQ TW_BK_MIGO-GJAHR_KEY
              AND BEWTP EQ 'Q'.


    IF NOT TW_MIRO IS INITIAL.
      FREE: IT_RBKP.
      SELECT * FROM RBKP
        INTO TABLE IT_RBKP
        FOR ALL ENTRIES IN TW_MIRO
        WHERE BELNR EQ TW_MIRO-BELNR.

      LOOP AT IT_RBKP INTO WA_RBKP WHERE NOT STBLG IS INITIAL.
        DELETE TW_MIRO WHERE BELNR EQ WA_RBKP-BELNR
                         AND GJAHR EQ WA_RBKP-GJAHR.
      ENDLOOP.

      DELETE IT_RBKP WHERE NOT STBLG IS INITIAL.

      IF IT_RBKP IS NOT INITIAL.
        SELECT BELNR ID_PROCESS
          FROM ZMMT0094
         APPENDING TABLE TW_SOFTEXPERT
         FOR ALL ENTRIES IN IT_RBKP
         WHERE BELNR EQ IT_RBKP-BELNR.
      ENDIF.

    ENDIF.

  ENDMETHOD.                    "M_SEL_DADOS_ALL

  METHOD M_SEL_DADOS_.

    FREE: TW_BS_MIGO, TW_BK_MIGO, TW_MIGO, TW_MIRO, TW_BS_MIGO_C.

    SELECT BUKRS BELNR GJAHR HKONT WRBTR SHKZG BUZEI ZUONR XBLNR
      FROM BSIS
        INTO TABLE TW_BS_MIGO
          WHERE BUKRS IN S_EMP
            AND BUDAT IN S_DTP
            AND ZUONR IN S_PED
            AND HKONT EQ CONTA.

    TW_BS_VBELN = TW_BS_MIGO.
    SORT TW_BS_VBELN BY ZUONR.
    DELETE ADJACENT DUPLICATES FROM TW_BS_VBELN COMPARING ZUONR.

    LOOP AT TW_BS_VBELN INTO W_BS_VBELN.
      LOOP AT TW_BS_MIGO INTO W_BS_MIGO WHERE ZUONR EQ W_BS_VBELN-ZUONR.

        W_BS_MIGO_C-ZUONR = W_BS_MIGO-ZUONR.
        W_BS_MIGO_C-HKONT = W_BS_MIGO-HKONT.
        W_BS_MIGO_C-GJAHR = W_BS_MIGO-GJAHR.

        CASE W_BS_MIGO-SHKZG.
          WHEN 'S'.  ADD      W_BS_MIGO-WRBTR TO   W_BS_MIGO_C-WRBTR.
          WHEN 'H'.  SUBTRACT W_BS_MIGO-WRBTR FROM W_BS_MIGO_C-WRBTR.
        ENDCASE.

      ENDLOOP.
      APPEND W_BS_MIGO_C TO TW_BS_MIGO_C.
      FREE: W_BS_MIGO, W_BS_MIGO_C.
    ENDLOOP.

    DELETE TW_BS_MIGO_C WHERE WRBTR NE 0.

    LOOP AT TW_BS_MIGO_C INTO W_BS_MIGO_C.
      DELETE TW_BS_MIGO WHERE ZUONR EQ W_BS_MIGO_C-ZUONR.
    ENDLOOP.

    CHECK NOT TW_BS_MIGO IS INITIAL.

    SELECT AWKEY BUKRS BELNR GJAHR
      FROM BKPF
        INTO TABLE TW_BK_MIGO
         FOR ALL ENTRIES IN TW_BS_MIGO
           WHERE BUKRS EQ TW_BS_MIGO-BUKRS
             AND BELNR EQ TW_BS_MIGO-BELNR
             AND GJAHR EQ TW_BS_MIGO-GJAHR.

    LOOP AT TW_BK_MIGO ASSIGNING <BK_MIGO>.
      <BK_MIGO>-BELNR_KEY = <BK_MIGO>-AWKEY(10).
      <BK_MIGO>-GJAHR_KEY = <BK_MIGO>-AWKEY+10(4).
    ENDLOOP.

  ENDMETHOD.                    "M_SEL_DADOS_


*  METHOD: M_SELECTS.
*
*    SELECT *
*      FROM SFLIGHT INTO TABLE IT_SS UP TO 100 ROWS
*      WHERE CARRID IN S_EMP AND
*            CONNID IN S_DTP.
*
*  ENDMETHOD.                    "ZCL_SELECIONA_DADOS

  METHOD M_AGRUPA_.

    SORT: TW_MIGO    BY AWKEY,
          TW_BK_MIGO BY AWKEY,
          TW_BS_MIGO BY BUKRS BELNR GJAHR,
          TW_MIRO    BY EBELN LFBNR GJAHR,
          TW_BK_MIRO BY AWKEY,
          TW_BS_MIRO BY BUKRS BELNR GJAHR,
          TW_ACEITE  BY EBELN BELNR.

    DATA: TABIX TYPE SY-TABIX.
    FREE: IT_SAIDA, IT_S_MIGO, IT_S_MIRO.

    IF TW_MIGO IS NOT INITIAL.
      SELECT EBELN ID_PROCESS
        INTO TABLE TW_SOFTEXPERT_PED
        FROM ZMMT0094
        FOR ALL ENTRIES IN TW_MIGO
       WHERE EBELN EQ TW_MIGO-EBELN.
    ENDIF.

    LOOP AT TW_MIGO ASSIGNING <MIGO>.

      TW_SAIDA-TO_MIGO = 0.
      TW_SAIDA-TO_MIRO = 0.

      MOVE: <MIGO>-EBELN TO TW_SAIDA-PEDIDO,
            <MIGO>-BELNR TO TW_SAIDA-NR_MIGO,
            <MIGO>-GJAHR TO TW_SAIDA-GJAHR_MG,
            <MIGO>-BUDAT TO TW_SAIDA-DT_MIGO.

      CLEAR WA_ACEITE.
      READ TABLE TW_ACEITE INTO WA_ACEITE WITH KEY EBELN = <MIGO>-EBELN
                                                   BELNR = <MIGO>-LFBNR.
      IF SY-SUBRC IS INITIAL.
        MOVE WA_ACEITE-BELNR TO TW_SAIDA-NR_FOLHA.
      ENDIF.

      LOOP AT TW_BK_MIGO ASSIGNING <BK_MIGO> WHERE AWKEY = <MIGO>-AWKEY.
        MOVE: <BK_MIGO>-BUKRS TO TW_SAIDA-EMPRESA.
        LOOP AT TW_BS_MIGO ASSIGNING <BS_MIGO> WHERE BUKRS = <BK_MIGO>-BUKRS
                                                 AND BELNR = <BK_MIGO>-BELNR
                                                 AND GJAHR = <BK_MIGO>-GJAHR.
          CASE <BS_MIGO>-SHKZG.
            WHEN 'S'.  ADD      <BS_MIGO>-WRBTR TO   TW_SAIDA-TO_MIGO.
            WHEN 'H'.  SUBTRACT <BS_MIGO>-WRBTR FROM TW_SAIDA-TO_MIGO.
          ENDCASE.
        ENDLOOP.
      ENDLOOP.

      READ TABLE TW_MIRO ASSIGNING <MIRO> WITH KEY LFBNR = <MIGO>-BELNR
                                                   GJAHR = <MIGO>-GJAHR
                                                   EBELN = <MIGO>-EBELN.
      IF SY-SUBRC IS INITIAL.

        MOVE: <MIRO>-EBELN TO TW_SAIDA-PEDIDO,
              <MIRO>-BELNR TO TW_SAIDA-NR_MIRO,
              <MIRO>-GJAHR TO TW_SAIDA-GJAHR_MR,
              <MIRO>-BUDAT TO TW_SAIDA-DT_MIRO,
              <MIRO>-GSBER TO TW_SAIDA-GSBER.

        CLEAR WA_ACEITE.
        READ TABLE TW_ACEITE INTO WA_ACEITE WITH KEY EBELN = <MIRO>-EBELN
                                                     BELNR = <MIRO>-LFBNR.
        IF SY-SUBRC IS INITIAL.
          MOVE WA_ACEITE-BELNR TO TW_SAIDA-NR_FOLHA.
        ENDIF.
        LOOP AT TW_BK_MIGO ASSIGNING <BK_MIRO> WHERE AWKEY = <MIRO>-AWKEY.
          MOVE: <BK_MIGO>-BUKRS TO TW_SAIDA-EMPRESA.

          LOOP AT TW_BS_MIGO ASSIGNING <BS_MIRO> WHERE BUKRS EQ <BK_MIRO>-BUKRS AND
                                                       BELNR EQ <BK_MIRO>-BELNR AND
                                                       GJAHR EQ <BK_MIRO>-GJAHR.
            CASE <BS_MIRO>-SHKZG.
              WHEN 'S'. ADD      <BS_MIRO>-WRBTR TO   TW_SAIDA-TO_MIRO.
              WHEN 'H'. SUBTRACT <BS_MIRO>-WRBTR FROM TW_SAIDA-TO_MIRO.
            ENDCASE.
          ENDLOOP.
        ENDLOOP.

      ELSE.

        READ TABLE TW_ACEITE ASSIGNING <ACEITE> WITH KEY BELNR = <MIGO>-LFBNR
                                                         EBELN = <MIGO>-EBELN.

        IF SY-SUBRC IS INITIAL.
          MOVE <ACEITE>-BELNR TO TW_SAIDA-NR_FOLHA.

          READ TABLE TW_MIRO ASSIGNING <MIRO> WITH KEY LFBNR = <MIGO>-LFBNR
                                                       GJAHR = <MIGO>-GJAHR
                                                       EBELN = <MIGO>-EBELN.
          IF SY-SUBRC IS INITIAL.
            MOVE: <MIRO>-EBELN TO TW_SAIDA-PEDIDO,
                  <MIRO>-BELNR TO TW_SAIDA-NR_MIRO.

            LOOP AT TW_BK_MIGO ASSIGNING <BK_MIRO> WHERE AWKEY = <MIRO>-AWKEY.
              MOVE: <BK_MIGO>-BUKRS TO TW_SAIDA-EMPRESA.

              LOOP AT TW_BS_MIGO ASSIGNING <BS_MIRO> WHERE BUKRS EQ <BK_MIRO>-BUKRS AND
                                                           BELNR EQ <BK_MIRO>-BELNR AND
                                                           GJAHR EQ <BK_MIRO>-GJAHR.
                CASE <BS_MIRO>-SHKZG.
                  WHEN 'S'. ADD      <BS_MIRO>-WRBTR TO   TW_SAIDA-TO_MIRO.
                  WHEN 'H'. SUBTRACT <BS_MIRO>-WRBTR FROM TW_SAIDA-TO_MIRO.
                ENDCASE.

              ENDLOOP.
            ENDLOOP.

          ENDIF.
        ENDIF.
      ENDIF.

      CLEAR: TW_SAIDA-ID_PROCESS.
      LOOP AT TW_SOFTEXPERT_PED INTO WA_SOFTEXPERT_PED WHERE EBELN EQ TW_SAIDA-PEDIDO.
        IF TW_SAIDA-ID_PROCESS IS INITIAL.
          TW_SAIDA-ID_PROCESS = WA_SOFTEXPERT_PED-ID_PROCESS.
        ELSE.
          CONCATENATE TW_SAIDA-ID_PROCESS ',' INTO TW_SAIDA-ID_PROCESS.
          CONCATENATE TW_SAIDA-ID_PROCESS WA_SOFTEXPERT_PED-ID_PROCESS INTO TW_SAIDA-ID_PROCESS SEPARATED BY SPACE.
        ENDIF.
      ENDLOOP.

      APPEND TW_SAIDA TO IT_SAIDA.
      FREE TW_SAIDA.

    ENDLOOP.

    LOOP AT TW_MIRO ASSIGNING <MIRO>.

      READ TABLE TW_SOFTEXPERT WITH KEY BELNR = <MIRO>-BELNR INTO WA_SOFTEXPERT.
      IF SY-SUBRC IS INITIAL.
        MOVE: WA_SOFTEXPERT-ID_PROCESS TO TW_SAIDA-ID_PROCESS.
      ENDIF.

      READ TABLE TW_MIGO ASSIGNING <MIGO> WITH KEY BELNR = <MIRO>-LFBNR
                                                   GJAHR = <MIRO>-GJAHR
                                                   EBELN = <MIRO>-EBELN.
      IF NOT SY-SUBRC IS INITIAL.

        READ TABLE TW_ACEITE ASSIGNING <ACEITE> WITH KEY BELNR = <MIRO>-LFBNR
                                                         EBELN = <MIRO>-EBELN.
        IF NOT SY-SUBRC IS INITIAL.

          MOVE: <MIRO>-EBELN TO TW_SAIDA-PEDIDO,
                <MIRO>-BELNR TO TW_SAIDA-NR_MIRO.

          LOOP AT TW_BK_MIGO ASSIGNING <BK_MIRO> WHERE AWKEY = <MIRO>-AWKEY.
            MOVE: <BK_MIGO>-BUKRS TO TW_SAIDA-EMPRESA.

            LOOP AT TW_BS_MIGO ASSIGNING <BS_MIRO> WHERE BUKRS EQ <BK_MIRO>-BUKRS AND
                                                         BELNR EQ <BK_MIRO>-BELNR AND
                                                         GJAHR EQ <BK_MIRO>-GJAHR.
              CASE <BS_MIRO>-SHKZG.
                WHEN 'S'. ADD      <BS_MIRO>-WRBTR TO   TW_SAIDA-TO_MIRO.
                WHEN 'H'. SUBTRACT <BS_MIRO>-WRBTR FROM TW_SAIDA-TO_MIRO.
              ENDCASE.

            ENDLOOP.
          ENDLOOP.

          APPEND TW_SAIDA TO IT_SAIDA.
          FREE TW_SAIDA.
        ENDIF.
      ENDIF.

    ENDLOOP.

    LOOP AT IT_SAIDA ASSIGNING <SAIDA>.

      <SAIDA>-DIFERENCA = <SAIDA>-TO_MIGO + <SAIDA>-TO_MIRO.
      SELECT SINGLE BSART
        INTO <SAIDA>-BSART
       FROM EKKO
       WHERE EBELN = <SAIDA>-PEDIDO.

      SELECT SINGLE WERKS
       INTO <SAIDA>-GSBER
      FROM EKPO
      WHERE EBELN = <SAIDA>-PEDIDO.

    ENDLOOP.

    DELETE IT_SAIDA WHERE DIFERENCA EQ 0.


  ENDMETHOD.                    "M_AGRUPA_


  METHOD M_AGRUPA.

    SORT: TW_MIGO    BY AWKEY,
          TW_BK_MIGO BY AWKEY,
          TW_BS_MIGO BY BUKRS BELNR GJAHR,
          TW_MIRO    BY EBELN LFBNR GJAHR,
          TW_BK_MIRO BY AWKEY,
          TW_BS_MIRO BY BUKRS BELNR GJAHR,
          TW_ACEITE  BY EBELN BELNR.

    DATA: TABIX TYPE SY-TABIX.
    FREE: IT_SAIDA, IT_S_MIGO, IT_S_MIRO.

    LOOP AT TW_MIGO ASSIGNING <MIGO>.

      TW_SAIDA-TO_MIGO = 0.
      TW_SAIDA-TO_MIRO = 0.

      IF  <MIGO>-BUKRS IS INITIAL.
        READ TABLE TW_ACEITE ASSIGNING <ACEITE> WITH KEY EBELN = <MIGO>-EBELN
                                                         BELNR = <MIGO>-LFBNR BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          MOVE: <ACEITE>-BUKRS TO TW_SAIDA-EMPRESA.
          MOVE: <ACEITE>-BUKRS TO <MIGO>-BUKRS.
        ENDIF.
      ELSE.
        MOVE: <MIGO>-BUKRS TO TW_SAIDA-EMPRESA.
      ENDIF.

      MOVE: <MIGO>-EBELN TO TW_SAIDA-PEDIDO,
            <MIGO>-BELNR TO TW_SAIDA-NR_MIGO.


      LOOP AT TW_BK_MIGO ASSIGNING <BK_MIGO> WHERE AWKEY = <MIGO>-AWKEY.
        LOOP AT TW_BS_MIGO ASSIGNING <BS_MIGO> WHERE BUKRS = <BK_MIGO>-BUKRS
                                                 AND BELNR = <BK_MIGO>-BELNR
                                                 AND GJAHR = <BK_MIGO>-GJAHR.
          CASE <BS_MIGO>-SHKZG.
            WHEN 'S'.  ADD      <BS_MIGO>-WRBTR TO   TW_SAIDA-TO_MIGO.
            WHEN 'H'.  SUBTRACT <BS_MIGO>-WRBTR FROM TW_SAIDA-TO_MIGO.
          ENDCASE.

        ENDLOOP.
      ENDLOOP.

      LOOP AT TW_MIRO ASSIGNING <MIRO> WHERE EBELN EQ <MIGO>-EBELN AND
                                             LFBNR EQ <MIGO>-LFBNR AND
                                             GJAHR EQ <MIGO>-GJAHR.
        MOVE <MIRO>-BELNR TO TW_SAIDA-NR_MIRO.
*
        LOOP AT TW_BK_MIRO ASSIGNING <BK_MIRO> WHERE AWKEY = <MIRO>-AWKEY.
          LOOP AT TW_BS_MIRO ASSIGNING <BS_MIRO> WHERE BUKRS EQ <BK_MIRO>-BUKRS AND
                                                       BELNR EQ <BK_MIRO>-BELNR AND
                                                       GJAHR EQ <BK_MIRO>-GJAHR.
            CASE <BS_MIRO>-SHKZG.
              WHEN 'S'. ADD      <BS_MIRO>-WRBTR TO   TW_SAIDA-TO_MIRO.
              WHEN 'H'. SUBTRACT <BS_MIRO>-WRBTR FROM TW_SAIDA-TO_MIRO.
            ENDCASE.

          ENDLOOP.
        ENDLOOP.
      ENDLOOP.

      APPEND TW_SAIDA TO IT_SAIDA.
      FREE TW_SAIDA.

    ENDLOOP.


    LOOP AT TW_MIGO_S_MIRO ASSIGNING <MIGO>.
      INSERT INITIAL LINE INTO TABLE IT_SAIDA ASSIGNING <SAIDA>.

      <SAIDA>-TO_MIGO = 0.
      CLEAR <SAIDA>.

      IF  <MIGO>-BUKRS IS INITIAL.
        READ TABLE TW_ACEITE ASSIGNING <ACEITE> WITH KEY EBELN = <MIGO>-EBELN
                                                         BELNR = <MIGO>-LFBNR BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          MOVE: <ACEITE>-BUKRS TO <SAIDA>-EMPRESA.
        ENDIF.
      ELSE.
        MOVE: <MIGO>-BUKRS TO <SAIDA>-EMPRESA.
      ENDIF.


      MOVE: <MIGO>-EBELN TO <SAIDA>-PEDIDO,
            <MIGO>-BELNR TO <SAIDA>-NR_MIGO.

      READ TABLE TW_BK_MIGO TRANSPORTING NO FIELDS WITH KEY AWKEY = <MIGO>-AWKEY BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        LOOP AT TW_BK_MIGO ASSIGNING <BK_MIGO> FROM SY-TABIX.
          IF <BK_MIGO>-AWKEY NE <MIGO>-AWKEY.
            EXIT.
          ENDIF.

          READ TABLE TW_BS_MIGO TRANSPORTING NO FIELDS WITH KEY BUKRS = <BK_MIGO>-BUKRS
                                                                BELNR = <BK_MIGO>-BELNR
                                                                GJAHR = <BK_MIGO>-GJAHR BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            LOOP AT TW_BS_MIGO ASSIGNING <BS_MIGO> FROM SY-TABIX.

              CASE <BS_MIGO>-SHKZG.
                WHEN 'S'.
                  ADD <BS_MIGO>-WRBTR TO <SAIDA>-TO_MIGO.
                WHEN OTHERS.
                  SUBTRACT <BS_MIGO>-WRBTR FROM <SAIDA>-TO_MIGO.
              ENDCASE.

              AT END OF GJAHR. EXIT. ENDAT.

            ENDLOOP.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDLOOP.


    LOOP AT IT_SAIDA ASSIGNING <SAIDA>.
      <SAIDA>-DIFERENCA = <SAIDA>-TO_MIGO + <SAIDA>-TO_MIRO.
    ENDLOOP.
    DELETE IT_SAIDA WHERE DIFERENCA EQ 0.


  ENDMETHOD.                    "M_AGRUPA

  METHOD M_CONCATENA.

*  CONCATENA BELNR E GJAHR PARA BUSCAR O BKPF
    LOOP AT TW_MIGO ASSIGNING <FS_BKPF>.
      CONCATENATE <FS_BKPF>-BELNR <FS_BKPF>-GJAHR INTO <FS_BKPF>-AWKEY.
    ENDLOOP.


*    CONCATENA BELNR E GJAHR PARA BUSCAR O BKPF
    LOOP AT TW_MIRO ASSIGNING <FS_BKPF>.
      CONCATENATE <FS_BKPF>-BELNR <FS_BKPF>-GJAHR INTO <FS_BKPF>-AWKEY.
    ENDLOOP.

* PREENCHE OS LFBNR QUANDO ESTIVER EM BRANCO PARA A ROTINA DE INSUMOS
    LOOP AT TW_MIRO ASSIGNING <MIRO> WHERE LFBNR IS INITIAL.
      READ TABLE IT_RBKP INTO WA_RBKP WITH KEY BELNR = <MIRO>-BELNR
                                               GJAHR = <MIRO>-GJAHR.
      IF  SY-SUBRC IS INITIAL.
        READ TABLE TW_MIGO ASSIGNING <MIGO> WITH KEY XBLNR = WA_RBKP-XBLNR
                                                     GJAHR = <MIRO>-GJAHR
                                                     EBELN = <MIRO>-EBELN.
        IF SY-SUBRC IS INITIAL.
          <MIRO>-XBLNR = <MIGO>-XBLNR.
          <MIRO>-LFBNR = <MIGO>-BELNR.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "M_CONCATENA

  METHOD: M_AGRUPA_DADOS.

    SORT: TW_MIGO    BY AWKEY,
          TW_BK_MIGO BY AWKEY,
          TW_BS_MIGO BY BUKRS BELNR GJAHR,
          TW_MIRO    BY EBELN LFBNR GJAHR,
          TW_BK_MIRO BY AWKEY,
          TW_BS_MIRO BY BUKRS BELNR GJAHR,
          TW_ACEITE  BY EBELN BELNR.

    DATA: TABIX TYPE SY-TABIX.
    FREE: IT_SAIDA, IT_S_MIGO, IT_S_MIRO.

    LOOP AT TW_MIGO ASSIGNING <MIGO>.
      INSERT INITIAL LINE INTO TABLE IT_SAIDA ASSIGNING <SAIDA>.

      <SAIDA>-TO_MIGO = 0.
      <SAIDA>-TO_MIRO = 0.

      IF  <MIGO>-BUKRS IS INITIAL.
        READ TABLE TW_ACEITE ASSIGNING <ACEITE> WITH KEY EBELN = <MIGO>-EBELN
                                                         BELNR = <MIGO>-LFBNR BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          MOVE: <ACEITE>-BUKRS TO <SAIDA>-EMPRESA.
          MOVE: <ACEITE>-BUKRS TO <MIGO>-BUKRS.
        ENDIF.
      ELSE.
        MOVE: <MIGO>-BUKRS TO <SAIDA>-EMPRESA.
      ENDIF.

      MOVE: <MIGO>-EBELN TO <SAIDA>-PEDIDO,
            <MIGO>-BELNR TO <SAIDA>-NR_MIGO,
            <MIGO>-GJAHR TO <SAIDA>-GJAHR_MG.

      READ TABLE TW_BK_MIGO TRANSPORTING NO FIELDS WITH KEY AWKEY = <MIGO>-AWKEY BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        LOOP AT TW_BK_MIGO ASSIGNING <BK_MIGO> FROM SY-TABIX.
          IF <BK_MIGO>-AWKEY NE <MIGO>-AWKEY.
            EXIT.
          ENDIF.

          READ TABLE TW_BS_MIGO TRANSPORTING NO FIELDS WITH KEY BUKRS = <BK_MIGO>-BUKRS
                                                                BELNR = <BK_MIGO>-BELNR
                                                                GJAHR = <BK_MIGO>-GJAHR BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            LOOP AT TW_BS_MIGO ASSIGNING <BS_MIGO> FROM SY-TABIX.

              CASE <BS_MIGO>-SHKZG.
                WHEN 'S'.
                  ADD <BS_MIGO>-WRBTR TO <SAIDA>-TO_MIGO.
                WHEN OTHERS.
                  SUBTRACT <BS_MIGO>-WRBTR FROM <SAIDA>-TO_MIGO.
              ENDCASE.

              AT END OF GJAHR. EXIT. ENDAT.

            ENDLOOP.
          ENDIF.
        ENDLOOP.
      ENDIF.

      READ TABLE TW_MIRO TRANSPORTING NO FIELDS WITH KEY EBELN = <MIGO>-EBELN
                                                         LFBNR = <MIGO>-LFBNR
                                                         GJAHR = <MIGO>-GJAHR BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        LOOP AT TW_MIRO ASSIGNING <MIRO> FROM SY-TABIX.
          MOVE: <MIRO>-BELNR TO <SAIDA>-NR_MIRO,
                <MIRO>-GJAHR TO <SAIDA>-GJAHR_MR.

          READ TABLE TW_SOFTEXPERT WITH KEY BELNR = <MIRO>-BELNR INTO WA_SOFTEXPERT.
          IF SY-SUBRC IS INITIAL.
            MOVE: WA_SOFTEXPERT-ID_PROCESS TO <SAIDA>-ID_PROCESS.
          ENDIF.

          READ TABLE TW_BK_MIRO TRANSPORTING NO FIELDS WITH KEY AWKEY = <MIRO>-AWKEY BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            LOOP AT TW_BK_MIRO ASSIGNING <BK_MIRO> FROM SY-TABIX.

*              IF <BK_MIRO>-AWKEY NE <MIRO>-AWKEY.
*                CONTINUE.
*              ENDIF.

              READ TABLE TW_BS_MIRO TRANSPORTING NO FIELDS WITH KEY BUKRS = <BK_MIRO>-BUKRS
                                                                    BELNR = <BK_MIRO>-BELNR
                                                                    GJAHR = <BK_MIRO>-GJAHR BINARY SEARCH.
              IF SY-SUBRC IS INITIAL.
                LOOP AT TW_BS_MIRO ASSIGNING <BS_MIRO> FROM SY-TABIX.

                  CASE <BS_MIRO>-SHKZG.
                    WHEN 'S'.
                      ADD <BS_MIRO>-WRBTR TO <SAIDA>-TO_MIRO.
                    WHEN OTHERS.
                      SUBTRACT <BS_MIRO>-WRBTR FROM <SAIDA>-TO_MIRO.
                  ENDCASE.
                  AT END OF GJAHR. EXIT. ENDAT.
                ENDLOOP.

              ENDIF.
            ENDLOOP.
          ENDIF.
          AT END OF GJAHR. EXIT. ENDAT.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    LOOP AT TW_MIGO_S_MIRO ASSIGNING <MIGO>.
      INSERT INITIAL LINE INTO TABLE IT_SAIDA ASSIGNING <SAIDA>.

      <SAIDA>-TO_MIGO = 0.
      CLEAR <SAIDA>.

      IF  <MIGO>-BUKRS IS INITIAL.
        READ TABLE TW_ACEITE ASSIGNING <ACEITE> WITH KEY EBELN = <MIGO>-EBELN
                                                         BELNR = <MIGO>-LFBNR BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          MOVE: <ACEITE>-BUKRS TO <SAIDA>-EMPRESA.
        ENDIF.
      ELSE.
        MOVE: <MIGO>-BUKRS TO <SAIDA>-EMPRESA.
      ENDIF.


      MOVE: <MIGO>-EBELN TO <SAIDA>-PEDIDO,
            <MIGO>-BELNR TO <SAIDA>-NR_MIGO,
            <MIGO>-GJAHR TO <SAIDA>-GJAHR_MG.

      READ TABLE TW_BK_MIGO TRANSPORTING NO FIELDS WITH KEY AWKEY = <MIGO>-AWKEY BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        LOOP AT TW_BK_MIGO ASSIGNING <BK_MIGO> FROM SY-TABIX.
          IF <BK_MIGO>-AWKEY NE <MIGO>-AWKEY.
            EXIT.
          ENDIF.

          READ TABLE TW_BS_MIGO TRANSPORTING NO FIELDS WITH KEY BUKRS = <BK_MIGO>-BUKRS
                                                                BELNR = <BK_MIGO>-BELNR
                                                                GJAHR = <BK_MIGO>-GJAHR BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            LOOP AT TW_BS_MIGO ASSIGNING <BS_MIGO> FROM SY-TABIX.

              CASE <BS_MIGO>-SHKZG.
                WHEN 'S'.
                  ADD <BS_MIGO>-WRBTR TO <SAIDA>-TO_MIGO.
                WHEN OTHERS.
                  SUBTRACT <BS_MIGO>-WRBTR FROM <SAIDA>-TO_MIGO.
              ENDCASE.

              AT END OF GJAHR. EXIT. ENDAT.

            ENDLOOP.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDLOOP.

    IT_SAIDA_AUX = IT_SAIDA.
    LOOP AT IT_SAIDA ASSIGNING <SAIDA>.
      <SAIDA>-DIFERENCA = <SAIDA>-TO_MIGO + <SAIDA>-TO_MIRO.
    ENDLOOP.
    DELETE IT_SAIDA WHERE DIFERENCA EQ 0.

  ENDMETHOD.                    "M_AGRUPA_DADOS

  METHOD: M_LAYOUT.


    CLEAR: WA_LAYOUT, WA_VARIANTE.

    WA_LAYOUT-ZEBRA      = ABAP_TRUE.
    WA_LAYOUT-NO_ROWINS  = ABAP_TRUE.
    WA_LAYOUT-STYLEFNAME = 'ESTILO'.
    WA_LAYOUT-INFO_FNAME = 'COLOR'.
    WA_LAYOUT-SEL_MODE   = 'C'.
    WA_STABLE-ROW        = ABAP_TRUE.

    WA_VARIANTE-REPORT  = SY-REPID.

*****    CLEAR: WA_LAYOUT, WA_VARIANTE.
*****
*****    WA_LAYOUT-ZEBRA      = ABAP_TRUE.
******    WA_LAYOUT-NO_ROWMARK = ABAP_TRUE.
*****    WA_LAYOUT-STYLEFNAME = 'STYLE'.
*****    WA_LAYOUT-INFO_FNAME = 'LINE_COLOR'.
******    WA_LAYOUT-CWIDTH_OPT = ABAP_TRUE.
*****
*****    WA_VARIANTE-REPORT  = SY-REPID. " Inclui a administração de Layout

  ENDMETHOD.                    "M_LAYOUT

  METHOD VERIFICAR_STATUS.

    FIELD-SYMBOLS <MM> TYPE TY_MM.

    ME->AT_TABLE = I_TABLE.

* INICIO ELIMINA OS ESTORNOS
    SORT ME->AT_TABLE BY EBELN LFBNR BELNR SHKZG.
    LOOP AT ME->AT_TABLE ASSIGNING <MM> WHERE SHKZG EQ 'H'.
      READ TABLE ME->AT_TABLE ASSIGNING <AUX> WITH KEY EBELN = <MM>-EBELN
                                                       LFBNR = <MM>-LFBNR
                                                       SHKZG = 'S'.
      CHECK SY-SUBRC IS INITIAL.
      <MM>-SHKZG   = 'W'.
      <AUX>-SHKZG  = 'W'.
    ENDLOOP.
    DELETE ME->AT_TABLE WHERE SHKZG EQ 'W'.
* FIM Elimina os Estornos

  ENDMETHOD.                    "M_VERIFICAR_STATUS

  METHOD M_ESTORNO.

    DATA: WA_SS_AUX TYPE SFLIGHT.
    DATA: WA_HEAD_RET TYPE BAPI2017_GM_HEAD_RET,
          IT_RETURN   TYPE TABLE OF BAPIRET2,
          TL_RETURN   TYPE TABLE OF BAPIRET2,
          WA_RETURN   TYPE BAPIRET2,
          REL_CODE    TYPE BAPIMMPARA-REL_CODE,
          BLDAT       TYPE IMKPF-BLDAT,
          SUBRC       TYPE SY-SUBRC,
          DATE        TYPE SY-DATUM,
          CONT        TYPE N LENGTH 2,
          VAR_EST     TYPE C VALUE ABAP_TRUE,
          MES         TYPE SY-DATUM,
          P_TEXTO     TYPE STRING.


    FREE: IT_SEL_ROWS[], WA_SEL_ROWS, IT_SAIDA_AUX.

    CALL METHOD WA_ALV->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = IT_SEL_ROWS.

    MOVE IT_SAIDA TO IT_SAIDA_AUX.

    CLEAR DATA.
    CALL SCREEN 0104 STARTING AT 050 3
                     ENDING   AT 100 5.

    CHECK NOT DATA IS INITIAL.

    BUDAT = |{ DATA+6(2) }.{ DATA+4(2) }.{ DATA+2(2) }|.
    BLDAT = BUDAT.

    LOOP AT IT_SEL_ROWS INTO WA_SEL_ROWS.
      READ TABLE IT_SAIDA_AUX INTO TW_SAIDA_AUX INDEX WA_SEL_ROWS-INDEX.

      CL_PROGRESS_INDICATOR=>PROGRESS_INDICATE( I_TEXT = |Processando PEDIDO nº { TW_SAIDA_AUX-PEDIDO }!| ).

      IF TW_SAIDA_AUX-NR_MIRO IS INITIAL.
        IF TW_SAIDA_AUX-NR_FOLHA IS INITIAL.
* Estorna a Migo
          CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
            EXPORTING
              MATERIALDOCUMENT    = TW_SAIDA_AUX-NR_MIGO
              MATDOCUMENTYEAR     = TW_SAIDA_AUX-GJAHR_MG
              GOODSMVT_PSTNG_DATE = DATA
            IMPORTING
              GOODSMVT_HEADRET    = WA_HEAD_RET
            TABLES
              RETURN              = IT_RETURN.

          WAIT UP TO 1 SECONDS.
          READ TABLE IT_RETURN INTO WA_RETURN WITH KEY TYPE = 'E'.
          IF NOT SY-SUBRC IS INITIAL.

            WAIT UP TO 2 SECONDS.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                WAIT = ABAP_TRUE.

*            CL_PROGRESS_INDICATOR=>PROGRESS_INDICATE( I_TEXT =  |Aplicando Modificações na Migo nº { TW_SAIDA_AUX-NR_MIGO }!| ).

            IF SY-SUBRC IS INITIAL.
              WA_RETURN-MESSAGE = |Estornando a MIGO { TW_SAIDA_AUX-NR_MIGO } !|.
              APPEND WA_RETURN TO TL_RETURN.
            ENDIF.

          ELSE.
            LOOP AT IT_RETURN INTO WA_RETURN.
              APPEND WA_RETURN TO TL_RETURN.
            ENDLOOP.
          ENDIF.
        ELSE.
* Estorna a Folha de Aceite
          FREE: IT_BDC, MESSTAB.

          PERFORM BATCH_INPUT USING:
                'X' 'SAPLMLSR'        '0400',
                ''  'BDC_OKCODE'      '=SELP',
                ''  'BDC_SUBSCR'      'SAPLMLSR                                0410SUB_HEADER',
                ''  'BDC_SUBSCR'      'SAPLMLSR                                0420SUB_ACCEPTANCE',
                ''  'BDC_SUBSCR'      'SAPLMLSR                                0450SUB_VALUES',
                ''  'BDC_SUBSCR'      'SAPLMLSR                                0430SUB_VENDOR',
                ''  'BDC_SUBSCR'      'SAPLMLSR                                0440SUB_ORIGIN',
                ''  'BDC_SUBSCR'      'SAPLMLSR                                0460SUB_HISTORY',
                ''  'BDC_SUBSCR'      'SAPLMLSR                                0330SUB_TEXT',
                ''  'BDC_SUBSCR'      'SAPLMLSP                                0400SERVICE',
                ''  'BDC_CURSOR'      'RM11P-NEW_ROW',
                ''  'RM11P-NEW_ROW'   '10',

                'X' 'SAPLMLSR'        '0340',
                ''  'BDC_CURSOR'      'RM11R-LBLNI',
                ''  'BDC_OKCODE'      '=ENTE',
                ''  'RM11R-LBLNI'     TW_SAIDA_AUX-NR_FOLHA,

                'X' 'SAPLMLSR'        '0400',
                ''  'BDC_OKCODE'      '=AKCH',
                ''  'BDC_SUBSCR'      'SAPLMLSR                                0410SUB_HEADER',
                ''  'BDC_SUBSCR'      'SAPLMLSR                                0420SUB_ACCEPTANCE',
                ''  'BDC_SUBSCR'      'SAPLMLSR                                0450SUB_VALUES',
                ''  'BDC_SUBSCR'      'SAPLMLSR                                0430SUB_VENDOR',
                ''  'BDC_SUBSCR'      'SAPLMLSR                                0440SUB_ORIGIN',
                ''  'BDC_SUBSCR'      'SAPLMLSR                                0460SUB_HISTORY',
                ''  'BDC_SUBSCR'      'SAPLMLSR                                0330SUB_TEXT',
                ''  'BDC_SUBSCR'      'SAPLMLSP                                0400SERVICE',
                ''  'BDC_CURSOR'      'ESLL-PERNR(01)',
                ''  'RM11P-NEW_ROW'	  '10',

                'X' 'SAPLMLSR'        '0400',
                ''  'BDC_OKCODE'      '=ACCR',
                ''  'BDC_SUBSCR'      'SAPLMLSR                                0410SUB_HEADER',
                ''  'BDC_SUBSCR'      'SAPLMLSR                                0420SUB_ACCEPTANCE',
                ''  'BDC_SUBSCR'      'SAPLMLSR                                0450SUB_VALUES',
                ''  'BDC_SUBSCR'      'SAPLMLSR                                0430SUB_VENDOR',
                ''  'BDC_SUBSCR'      'SAPLMLSR                                0440SUB_ORIGIN',
                ''  'BDC_SUBSCR'      'SAPLMLSR                                0460SUB_HISTORY',
                ''  'BDC_SUBSCR'      'SAPLMLSR                                0330SUB_TEXT',
                ''  'BDC_SUBSCR'      'SAPLMLSP                                0400SERVICE',
                ''  'BDC_CURSOR'      'ESLL-PERNR(01)',
                ''  'RM11P-NEW_ROW'	  '10',

                'X' 'SAPLMLSR'        '0400',
                ''  'BDC_OKCODE'      '=SAVE',
                ''  'BDC_SUBSCR'      'SAPLMLSR                                0410SUB_HEADER',
                ''  'BDC_SUBSCR'      'SAPLMLSR                                0420SUB_ACCEPTANCE',
                ''  'BDC_SUBSCR'      'SAPLMLSR                                0450SUB_VALUES',
                ''  'BDC_SUBSCR'      'SAPLMLSR                                0430SUB_VENDOR',
                ''  'BDC_SUBSCR'      'SAPLMLSR                                0440SUB_ORIGIN',
                ''  'BDC_SUBSCR'      'SAPLMLSR                                0460SUB_HISTORY',
                ''  'BDC_SUBSCR'      'SAPLMLSR                                0330SUB_TEXT',
                ''  'BDC_SUBSCR'      'SAPLMLSP                                0400SERVICE',
                ''  'BDC_CURSOR'      'ESLL-PERNR(01)',
                ''  'RM11P-NEW_ROW'	  '10',

                'X' 'SAPLSPO1'        '0300',
                '' 	'BDC_OKCODE'     	'=YES',

                'X' 'SAPLMLSR'        '0110',
                ''  'BDC_CURSOR'      'IMKPF-BUDAT',
                ''  'BDC_OKCODE'      '=ENTE',
                ''  'IMKPF-BLDAT'     BLDAT,
                ''  'IMKPF-BUDAT'	    BUDAT.

*      OPT-DISMODE = 'E'.
          WL_MODE = 'N'.
          CALL TRANSACTION 'ML81N' USING IT_BDC MODE  WL_MODE MESSAGES INTO MESSTAB.
          WAIT UP TO 2 SECONDS.

          READ TABLE MESSTAB INTO WMESSTAB WITH KEY MSGTYP = 'E'.
          IF SY-SUBRC IS INITIAL.
            WA_RETURN-MESSAGE = WMESSTAB-MSGV1.
            WA_RETURN-TYPE = WMESSTAB-MSGTYP.
            APPEND WA_RETURN TO TL_RETURN.
          ELSE.

            CALL FUNCTION 'BAPI_ENTRYSHEET_DELETE'
              EXPORTING
                ENTRYSHEET = TW_SAIDA_AUX-NR_FOLHA
              TABLES
                RETURN     = IT_RETURN.

            WAIT UP TO 2 SECONDS.

            READ TABLE IT_RETURN INTO WA_RETURN WITH KEY TYPE = 'E'.
            IF NOT SY-SUBRC IS INITIAL.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  WAIT = ABAP_TRUE.

              WAIT UP TO 2 SECONDS.

              IF SY-SUBRC IS INITIAL.
                WA_RETURN-MESSAGE = |Estornando a Folha { TW_SAIDA_AUX-NR_FOLHA } !|.
                APPEND WA_RETURN TO TL_RETURN.
              ENDIF.

            ELSE.
              LOOP AT IT_RETURN INTO WA_RETURN.
                APPEND WA_RETURN TO TL_RETURN.
              ENDLOOP.
            ENDIF.

          ENDIF.

        ENDIF.

        READ TABLE IT_RETURN INTO WA_RETURN WITH KEY TYPE = 'I'.
        IF SY-SUBRC IS INITIAL.
          APPEND WA_RETURN TO TL_RETURN.
        ENDIF.
      ELSE.
        WA_RETURN-MESSAGE = |Existem MIRO para este Documento!|.
        WA_RETURN-TYPE = 'E'.
        APPEND WA_RETURN TO TL_RETURN.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_STRUCTURE_NAME      = 'BAPIRET2'
        I_SAVE                = 'A'
        I_SCREEN_START_COLUMN = 3
        I_SCREEN_START_LINE   = 3
        I_SCREEN_END_COLUMN   = 100
        I_SCREEN_END_LINE     = 13
      TABLES
        T_OUTTAB              = TL_RETURN.

  ENDMETHOD.                    "M_ESTORNO

  METHOD SHDB.

  ENDMETHOD.

  METHOD TIME_OUT.

    P_TEXTO = I_TEXTO.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        TEXT = P_TEXTO.

  ENDMETHOD.

ENDCLASS.                    "ZCL_SELECIONA_DADOS IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ZCL_ALV_TOOLBAR IMPLEMENTATION.

  METHOD CONSTRUCTOR.

    CREATE OBJECT C_ALV_TM
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.

  ENDMETHOD.                    "CONSTRUCTOR

  METHOD ON_TOOLBAR.
*
*    TY_TOOLBAR-BUTN_TYPE = 3.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-ICON      =  ICON_DELETE_ROW.
    TY_TOOLBAR-TEXT      = TEXT-002.
    TY_TOOLBAR-FUNCTION  =  'DEL'.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-BUTN_TYPE = 3.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

*   variable for Toolbar Button
    TY_TOOLBAR-ICON      = ICON_VIEW_CLOSE.
    TY_TOOLBAR-FUNCTION  = 'CLOS_MSG'.
    TY_TOOLBAR-DISABLED  = SPACE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

*    CALL METHOD C_ALV_TM->REORGANIZE
*      EXPORTING
*        IO_ALV_TOOLBAR = E_OBJECT.


  ENDMETHOD.                    "ON_TOOLBAR

  METHOD HANDLE_ESTORNO.

    DATA OBJ_REL TYPE REF TO ZCL_RELATORIO_CCTM.
    CREATE OBJECT OBJ_REL.

    CASE E_UCOMM.
      WHEN 'DEL'.
        OBJ_REL->M_ESTORNO( ).
    ENDCASE.

  ENDMETHOD.                    "HANDLE_ESTORNO

  METHOD ON_DOUBLE_CLICK.

    DATA OBJ_REL TYPE REF TO ZCL_RELATORIO_CCTM.
    CREATE OBJECT OBJ_REL.
    OBJ_REL->M_ESTORNO( ).

  ENDMETHOD.                    "ON_DOUBLE_CLICK

  METHOD ON_CLICK.

    DATA OPT TYPE CTU_PARAMS.
    DATA OBJ_REL TYPE REF TO ZCL_RELATORIO_CCTM.
    CREATE OBJECT OBJ_REL.

    FREE TL_BDC.
    READ TABLE IT_SAIDA INTO DATA(WA_SAIDA) INDEX E_ROW_ID.

    CASE E_COLUMN_ID.
      WHEN 'PEDIDO'.

        CHECK NOT WA_SAIDA-PEDIDO IS INITIAL.

        SET PARAMETER ID 'BES' FIELD WA_SAIDA-PEDIDO.
        CALL TRANSACTION  'ME23N' AND SKIP FIRST SCREEN.

      WHEN 'NR_MIGO'.

        CHECK NOT WA_SAIDA-NR_MIGO IS INITIAL.

        PERFORM F_PREENCHER_DYNPRO USING:
                'X' 'SAPLMIGO'            '0001',
                ' ' 'BDC_OKCODE'          '=MIGO_OK_GO',
                ' ' 'BDC_SUBSCR'          'SAPLMIGO',
                ' ' 'GODYNPRO-ACTION'     'A04',
                ' ' 'GODYNPRO-REFDOC'     'R02',
                ' ' 'BDC_SUBSCR'          'SAPLMIGO',
                ' ' 'BDC_CURSOR'          'GODYNPRO-MAT_DOC',
                ' ' 'GODYNPRO-MAT_DOC'    WA_SAIDA-NR_MIGO,
                ' ' 'GODYNPRO-DOC_YEAR'   WA_SAIDA-GJAHR_MG,
                ' ' 'BDC_SUBSCR'          'SAPLMIGO'.

        OPT-DISMODE = 'E'.
        OPT-DEFSIZE = ' '.

        CALL TRANSACTION 'MIGO' USING TL_BDC OPTIONS FROM OPT.

      WHEN 'NR_MIRO'.

        CHECK NOT WA_SAIDA-NR_MIRO IS INITIAL.

        SET PARAMETER ID 'RBN' FIELD WA_SAIDA-NR_MIRO.
        SET PARAMETER ID 'GJR' FIELD WA_SAIDA-GJAHR_MR.
        CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.                    "LCL_ALV_TOOLBAR DEFINITION

*&---------------------------------------------------------------------*
*&      Form  F_PREENCHER_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2594   text
*      -->P_2595   text
*      -->P_2596   text
*----------------------------------------------------------------------*
FORM F_PREENCHER_DYNPRO USING L_START TYPE C L_NAME TYPE C L_VALUE.

  MOVE L_START TO WL_BDC-DYNBEGIN.
  IF L_START = 'X'.
    MOVE:
  L_NAME  TO WL_BDC-PROGRAM,
  L_VALUE TO WL_BDC-DYNPRO.
  ELSE.
    MOVE:
      L_NAME  TO WL_BDC-FNAM,
      L_VALUE TO WL_BDC-FVAL.
  ENDIF.
  APPEND WL_BDC TO TL_BDC.
  CLEAR: WL_BDC.


ENDFORM.
