************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação - ABAP               *
* Data desenv ...: 04.07.2008                                          *
* Tipo de prg ...: executável                                          *
* Objetivo    ...: Impressão de etiqueta de materiais                  *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 04.07.2008    Michely              Criação              DEVK904422   *
* 22.08.2008    Marcus Barbara       Alteração            DEVK904768   *
* 03.09.2008    Marcus Barbara       Alteração            DEVK904868   *
* 10.03.2009    Marcus Barbara       Alteração            DEVK905629   *
* 08.02.2009    RFREITAS             Alteração            DEVK9A1D8D   *
************************************************************************

REPORT  zmmr009.

*----------------------------------------------------------------------*
* Tabelas                                                              *
*----------------------------------------------------------------------*
TABLES: mard.

*----------------------------------------------------------------------*
* Tabelas Internas                                                     *
*----------------------------------------------------------------------*
DATA: BEGIN OF wa_mard OCCURS 0,
        matnr      LIKE mard-matnr,
        werks      LIKE mard-werks,
        lgort      LIKE mard-lgort,
        lgpbe      LIKE mard-lgpbe,
        maktx      LIKE makt-maktx,
        codigo(26),
      END  OF wa_mard,

      BEGIN OF gw_data OCCURS 0,
        matnr TYPE string,
        maktx TYPE string,
        werks TYPE string,
        lgort TYPE string,
        lgpbe TYPE string,
      END OF gw_data,

      BEGIN OF wa_linhas,
        linha TYPE c LENGTH 150,
      END OF wa_linhas.

DATA: "GW_DATA            TYPE ZMME_ETIQUETAS,
*      WA_ETIQUETAS            TYPE ZMME_ETIQUETAS,
*      IT_ETIQUETAS       LIKE STANDARD TABLE OF WA_ETIQUETAS,
  it_mard   LIKE STANDARD TABLE OF wa_mard,
  it_linhas LIKE STANDARD TABLE OF wa_linhas.

DEFINE preenche_linha.
  wa_linhas = &1.
  APPEND wa_linhas TO it_linhas.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
* Variávei globais
*----------------------------------------------------------------------*
DATA: vg_fm_name         TYPE rs38l_fnam. "Nome da função smart form

*----------------------------------------------------------------------*
* Tela de seleção
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-s02.
  SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-s00.
    SELECT-OPTIONS:
        s_matnr            FOR mard-matnr,
        s_werks            FOR mard-werks,
        s_lgort            FOR mard-werks,
        s_lgpbe            FOR mard-lgpbe.
  SELECTION-SCREEN END   OF BLOCK b0.

  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s01.
    PARAMETERS:
      r_col5 RADIOBUTTON GROUP tp DEFAULT 'X', "Modelo New tam p
      r_col6 RADIOBUTTON GROUP tp,             "Modelo New tam g
      r_col3 RADIOBUTTON GROUP tp, "DEFAULT 'X',
      r_col1 RADIOBUTTON GROUP tp,
      r_col2 RADIOBUTTON GROUP tp,
      r_col4 RADIOBUTTON GROUP tp.
  SELECTION-SCREEN END   OF BLOCK b1.


  SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-s03.
    PARAMETERS:
      p_rimp   AS CHECKBOX DEFAULT '',
      p_rimp_q TYPE i.
  SELECTION-SCREEN END   OF BLOCK b3.

***OAY-Rimini-Inicio-16.02.2022-IR084828
  SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-s04.
    PARAMETERS:
      r_lpt RADIOBUTTON GROUP pr DEFAULT 'X', "Impressora Paralela
      r_usb RADIOBUTTON GROUP pr.             "Impressora USB
  SELECTION-SCREEN END   OF BLOCK b4.
***OAY-Rimini-Fim-16.02.2022-IR084828
SELECTION-SCREEN END   OF BLOCK b2.
*----------------------------------------------------------------------*
* Start-Of-Selection                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Busco dados do material
  PERFORM f_busca_dados.
* Gero código de barra
  PERFORM f_gera_cod_bar.
* Chamo Smart para emissão das etiquetas
  PERFORM f_chama_etiquetas.
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_busca_dados .

  DATA: vl_lines  TYPE i,
        vl_copias TYPE i.

*  FREE: IT_ETIQUETAS.
  SELECT mard~matnr mard~werks  mard~lgort mard~lgpbe
         makt~maktx
    FROM mard INNER JOIN makt ON
       ( mard~matnr EQ makt~matnr )
    INTO TABLE it_mard
   WHERE mard~matnr IN s_matnr
     AND mard~werks IN s_werks
     AND mard~lgort IN s_lgort
     AND mard~lgpbe IN s_lgpbe
     AND makt~spras EQ sy-langu.

  IF sy-subrc <> 0.
    MESSAGE i000(z01) WITH 'Não há dados para seleção.'.
    STOP.
  ELSE.
    SORT it_mard BY matnr.
    DELETE ADJACENT DUPLICATES FROM it_mard COMPARING matnr.
  ENDIF.

  "Verifica se solicitou cópia da etiqueta do mesmo material.
  "Caso sim, obriga que se informe apenas um material.
  IF ( p_rimp EQ 'X' ).

    IF p_rimp_q  <= 0.
      MESSAGE i000(z01) WITH 'Favor informar a quantidade de cópias!'.
      STOP.
    ENDIF.

    vl_lines = lines( it_mard ).

    IF vl_lines NE 1.
      MESSAGE i000(z01) WITH 'Favor informar apenas um material!'.
      STOP.
    ENDIF.

    "Gera linhas de Cópia do mesmo registro
    READ TABLE it_mard INTO wa_mard INDEX 1.

    vl_copias = p_rimp_q - 1.
    DO vl_copias TIMES.
      APPEND wa_mard TO it_mard.
    ENDDO.

  ENDIF.


ENDFORM.                    " F_BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_GERA_COD_BAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_gera_cod_bar .
  DATA: vl_mod         TYPE i,
        vl_leng        TYPE i,
*-CS1092881-#RIMINI-05.15.2023-BEGIN
        vl_leng_centro TYPE i,
*-CS1092881-#RIMINI-05.15.2023-END
        ln_matnr(10)   TYPE n,
        centro         TYPE t001w-name1,
        vl_texto       LIKE wa_mard-maktx.

  CHECK NOT it_mard[] IS INITIAL.

  LOOP AT it_mard INTO wa_mard.
    CLEAR vl_texto.

    SHIFT wa_mard-matnr LEFT DELETING LEADING '0'.

* Ini - RJF - CS2022000524 Criação novo layout para etiqueta de materiais - 78400 - 18.01.2023
    IF ( r_col6 EQ 'X' ). " RJF antigo

      preenche_linha 'N'.
      preenche_linha '0'.
      preenche_linha 'S2'.

      preenche_linha 'Q614,27'.
      preenche_linha 'q784'.

*        CONCATENATE 'B50,40,0,3,2,7,150,B,"' wa_mard-matnr '"' INTO wa_linhas-linha.
*        APPEND wa_linhas TO it_linhas.

*      SELECT SINGLE name1 INTO centro
*        FROM t001w
*       WHERE werks EQ wa_mard-werks.

      SELECT SINGLE gtext INTO centro
          FROM tgsbt
        WHERE spras EQ sy-langu
          AND gsber EQ wa_mard-werks.

      "// Verifica se o tamanho da descrição é maior que 18
      "// para complementar na linha inferior da etiqueta.
      vl_leng = strlen( wa_mard-maktx ).
      IF ( vl_leng > 20 ).
        vl_leng        = vl_leng - 20.
        vl_texto       = wa_mard-maktx+20(vl_leng).
      ENDIF.

      CONCATENATE wa_mard-werks
                  '-' centro INTO DATA(lv_centroc)
        SEPARATED BY space.

      CONCATENATE 'A10,40,0,4,1,1,N,"' lv_centroc '"' INTO wa_linhas-linha.
      APPEND wa_linhas TO it_linhas.

      ln_matnr = wa_mard-matnr.
      CONCATENATE 'A10,100,0,5,1,1,N,"' ln_matnr '"' INTO wa_linhas-linha.
      APPEND wa_linhas TO it_linhas.

      CONCATENATE 'A10,210,0,5,1,1,N,"' wa_mard-maktx(20) '"' INTO wa_linhas-linha.
      APPEND wa_linhas TO it_linhas.

      IF vl_texto(1) IS INITIAL.
        vl_texto = vl_texto+1(19).
      ENDIF.

      CONCATENATE 'A10,320,0,5,1,1,N,"' vl_texto(20) '"' INTO wa_linhas-linha.
      APPEND wa_linhas TO it_linhas.

      CONCATENATE wa_mard-lgort
                  '-' wa_mard-lgpbe INTO DATA(lv_lgortc) SEPARATED BY space.

      CONCATENATE 'A10,440,0,5,1,1,N,"' lv_lgortc '"' INTO wa_linhas-linha.
      APPEND wa_linhas TO it_linhas.

*      CONCATENATE 'A50,320,0,3,1,1,N,"Centro: ' wa_mard-werks ' - ' ' ' centro '"' INTO wa_linhas-linha.
*      APPEND wa_linhas TO it_linhas.

*      CONCATENATE 'A50,360,0,3,1,1,N,"Deposito: ' wa_mard-lgort '"' INTO wa_linhas-linha.
*      APPEND wa_linhas TO it_linhas.
*
*      CONCATENATE 'A50,400,0,3,1,1,N,"Endereco: ' wa_mard-lgpbe '"' INTO wa_linhas-linha.
*      APPEND wa_linhas TO it_linhas.

      preenche_linha 'P1'.

    ELSEIF ( r_col5 EQ 'X' ).  " RJF novo

      vl_mod = sy-tabix MOD 2.

      "// Verifica se o tamanho da descrição é maior que 18
      "// para complementar na linha inferior da etiqueta.
      vl_leng = strlen( wa_mard-maktx ).
      IF ( vl_leng > 20 ).
        vl_leng        = vl_leng - 20.
        vl_texto       = wa_mard-maktx+20(vl_leng).
      ENDIF.

      "//A impressão das etiquetas atende as seguintes regras:
      "//Caso a linha for impar imprime na 1º etiqueta;
      "//Caso a linha for par imprime da 2º etiqueta;

      IF ( vl_mod = 1 ).
        preenche_linha 'N'.
        preenche_linha '0'.
        preenche_linha 'S2'.
*
        preenche_linha 'Q26,0'.
        preenche_linha 'q810'.

        SELECT SINGLE gtext INTO centro
            FROM tgsbt
          WHERE spras EQ sy-langu
            AND gsber EQ wa_mard-werks.

        CONCATENATE wa_mard-werks
                    '-' centro INTO lv_centroc
          SEPARATED BY space.
*-CS1092881-#RIMINI-05.15.2023-BEGIN
        vl_leng_centro = strlen( lv_centroc ).

        IF vl_leng_centro GT 20.
*-CS1092881-#RIMINI-05.15.2023-END
          CONCATENATE 'A30,70,0,4,1,0,N,"' lv_centroc(20) '"' INTO wa_linhas-linha. "180
*-CS1092881-#RIMINI-05.15.2023-BEGIN
        ELSE.
          CONCATENATE 'A30,70,0,4,1,0,N,"' lv_centroc '"' INTO wa_linhas-linha. "180
        ENDIF.
*-CS1092881-#RIMINI-05.15.2023-END
        APPEND wa_linhas TO it_linhas.

        ln_matnr = wa_mard-matnr.
        CONCATENATE 'A30,100,0,5,0,0,N,"'      ln_matnr '"' INTO wa_linhas.
        APPEND wa_linhas TO it_linhas.

        CONCATENATE 'A30,160,0,3,0,0,N,"' wa_mard-maktx(20) '"' INTO wa_linhas.
        APPEND wa_linhas TO it_linhas.

        CONCATENATE 'A30,190,0,3,0,0,N,"' vl_texto(20) '"' INTO wa_linhas.
        APPEND wa_linhas TO it_linhas.

        SELECT SINGLE name1 INTO centro
          FROM t001w
         WHERE werks EQ wa_mard-werks.

        CONCATENATE wa_mard-lgort '-' wa_mard-lgpbe INTO lv_lgortc SEPARATED BY space.

        CONCATENATE 'A30,220,0,4,1,0,N,"' lv_lgortc '"' INTO wa_linhas-linha. "200
        APPEND wa_linhas TO it_linhas.

      ELSE.
        SELECT SINGLE gtext INTO centro
            FROM tgsbt
          WHERE spras EQ sy-langu
            AND gsber EQ wa_mard-werks.

        CONCATENATE wa_mard-werks
                    '-' centro INTO lv_centroc
          SEPARATED BY space.
*-CS1092881-#RIMINI-05.15.2023-BEGIN
        vl_leng_centro = strlen( lv_centroc ).

        IF vl_leng_centro GT 20.
*-CS1092881-#RIMINI-05.15.2023-END
          CONCATENATE 'A450,70,0,4,1,0,N,"' lv_centroc(20) '"' INTO wa_linhas-linha.
*-CS1092881-#RIMINI-05.15.2023-BEGIN
        ELSE.
          CONCATENATE 'A450,70,0,4,1,0,N,"' lv_centroc '"' INTO wa_linhas-linha.
        ENDIF.
*-CS1092881-#RIMINI-05.15.2023-END
        APPEND wa_linhas TO it_linhas.

        ln_matnr = wa_mard-matnr.

        CONCATENATE 'A450,100,0,5,0,0,N,"'     ln_matnr '"' INTO wa_linhas.
        APPEND wa_linhas TO it_linhas.

        CONCATENATE 'A450,160,0,4,0,0,N,"' wa_mard-maktx(20) '"' INTO wa_linhas.
        APPEND wa_linhas TO it_linhas.

        CONCATENATE 'A450,190,0,4,0,0,N,"' vl_texto(20) '"' INTO wa_linhas.
        APPEND wa_linhas TO it_linhas.

        SELECT SINGLE name1 INTO centro
          FROM t001w
         WHERE werks EQ wa_mard-werks.

        CONCATENATE wa_mard-lgort '-' wa_mard-lgpbe INTO lv_lgortc SEPARATED BY space.

        CONCATENATE 'A450,220,0,4,1,0,N,"' lv_lgortc '"' INTO wa_linhas-linha.
        APPEND wa_linhas TO it_linhas.

        preenche_linha 'P1'.

      ENDIF.


    ELSEIF ( r_col1 EQ 'X' )
    OR ( r_col2 EQ 'X' ).
      "ALRS

      IF r_col2 = 'X'.
        preenche_linha 'N'.
        preenche_linha '0'.
        preenche_linha 'S2'.

        preenche_linha 'Q519,022'.
        preenche_linha 'q831'.

        CONCATENATE 'B50,90,0,3,2,7,150,B,"' wa_mard-matnr '"' INTO wa_linhas-linha.
        APPEND wa_linhas TO it_linhas.

        CONCATENATE 'A50,330,0,3,1,1,N,"Material: ' wa_mard-maktx '"' INTO wa_linhas-linha.
        APPEND wa_linhas TO it_linhas.

        SELECT SINGLE name1 INTO centro
          FROM t001w
         WHERE werks EQ wa_mard-werks.

        CONCATENATE 'A52,370,0,3,1,1,N,"Centro: ' wa_mard-werks ' - ' centro '"' INTO wa_linhas-linha.
        APPEND wa_linhas TO it_linhas.

        CONCATENATE 'A52,410,0,3,1,1,N,"Deposito: ' wa_mard-lgort '"' INTO wa_linhas-linha.
        APPEND wa_linhas TO it_linhas.

        CONCATENATE 'A52,450,0,3,1,1,N,"Endereco: ' wa_mard-lgpbe '"' INTO wa_linhas-linha.
        APPEND wa_linhas TO it_linhas.
      ELSE.
        preenche_linha 'N'.
        preenche_linha '0'.
        preenche_linha 'S2'.

        preenche_linha 'Q614,27'.
        preenche_linha 'q784'.

        CONCATENATE 'B50,40,0,3,2,7,150,B,"' wa_mard-matnr '"' INTO wa_linhas-linha.
        APPEND wa_linhas TO it_linhas.

        CONCATENATE 'A50,280,0,3,1,1,N,"Material: ' wa_mard-maktx '"' INTO wa_linhas-linha.
        APPEND wa_linhas TO it_linhas.

        SELECT SINGLE name1 INTO centro
          FROM t001w
         WHERE werks EQ wa_mard-werks.

        CONCATENATE 'A50,320,0,3,1,1,N,"Centro: ' wa_mard-werks ' - ' centro '"' INTO wa_linhas-linha.
        APPEND wa_linhas TO it_linhas.

        CONCATENATE 'A50,360,0,3,1,1,N,"Deposito: ' wa_mard-lgort '"' INTO wa_linhas-linha.
        APPEND wa_linhas TO it_linhas.

        CONCATENATE 'A50,400,0,3,1,1,N,"Endereco: ' wa_mard-lgpbe '"' INTO wa_linhas-linha.
        APPEND wa_linhas TO it_linhas.
      ENDIF.

      preenche_linha 'P1'.

    ELSE.
      vl_mod = sy-tabix MOD 2.

      "// Verifica se o tamanho da descrição é maior que 18
      "// para complementar na linha inferior da etiqueta.
      vl_leng = strlen( wa_mard-maktx ).
      IF ( vl_leng > 16 ).
        vl_leng        = vl_leng - 16.
        vl_texto       = wa_mard-maktx+16(vl_leng).
      ENDIF.

      "//A impressão das etiquetas atende as seguintes regras:
      "//Caso a linha for impar imprime na 1º etiqueta;
      "//Caso a linha for par imprime da 2º etiqueta;

      IF ( vl_mod = 1 ).
        preenche_linha 'N'.
        preenche_linha '0'.
        preenche_linha 'S2'.
*
        preenche_linha 'Q25,0'.
        preenche_linha 'q810'.

        CONCATENATE 'B30,30,0,3,2,1,50,B,"' wa_mard-matnr '"' INTO wa_linhas.
        APPEND wa_linhas TO it_linhas.

        CONCATENATE 'A30,120,0,3,0,0,N,"Material: ' wa_mard-maktx(16) '"' INTO wa_linhas.
        APPEND wa_linhas TO it_linhas.

        CONCATENATE 'A30,145,0,3,0,0,N,"' vl_texto(27) '"' INTO wa_linhas.
        APPEND wa_linhas TO it_linhas.

        SELECT SINGLE name1 INTO centro
          FROM t001w
         WHERE werks EQ wa_mard-werks.

        CONCATENATE 'A30,180,0,2,1,0,N,"Centro: ' wa_mard-werks  '-' centro(16) '"' INTO wa_linhas-linha.
        APPEND wa_linhas TO it_linhas.

        CONCATENATE 'A30,200,0,2,1,0,N,"Deposito: ' wa_mard-lgort '"' INTO wa_linhas-linha.
        APPEND wa_linhas TO it_linhas.

        CONCATENATE 'A30,220,0,2,1,0,N,"Endereco: ' wa_mard-lgpbe '"' INTO wa_linhas-linha.
        APPEND wa_linhas TO it_linhas.

      ELSE.
        CONCATENATE 'B450,30,0,3,2,1,50,B,"' wa_mard-matnr '"' INTO wa_linhas.
        APPEND wa_linhas TO it_linhas.

        CONCATENATE 'A450,120,0,3,0,0,N,"Material: ' wa_mard-maktx(16) '"' INTO wa_linhas.
        APPEND wa_linhas TO it_linhas.

        CONCATENATE 'A450,145,0,3,0,0,N,"' vl_texto(27) '"' INTO wa_linhas.
        APPEND wa_linhas TO it_linhas.

        SELECT SINGLE name1 INTO centro
          FROM t001w
         WHERE werks EQ wa_mard-werks.

        CONCATENATE 'A450,180,0,2,1,0,N,"Centro: ' wa_mard-werks ' - ' centro(16) '"' INTO wa_linhas-linha.
        APPEND wa_linhas TO it_linhas.

        CONCATENATE 'A450,200,0,2,1,0,N,"Deposito: ' wa_mard-lgort '"' INTO wa_linhas-linha.
        APPEND wa_linhas TO it_linhas.

        CONCATENATE 'A450,220,0,2,1,0,N,"Endereco: ' wa_mard-lgpbe '"' INTO wa_linhas-linha.
        APPEND wa_linhas TO it_linhas.

        preenche_linha 'P1'.
      ENDIF.
    ENDIF.
*      vl_leng = strlen( wa_mard-maktx ).
*      IF ( vl_leng > 15 ).
*        vl_leng        = vl_leng - 15.
*        vl_texto       = wa_mard-maktx+15(vl_leng).
*      ENDIF.
*
*      "//A impressão das etiquetas atende as seguintes regras:
*      "//Caso a linha for impar imprime na 1º etiqueta;
*      "//Caso a linha for par imprime da 2º etiqueta;
*
*      IF ( vl_mod = 1 ).
*        preenche_linha 'N'.
*        preenche_linha '0'.
*        preenche_linha 'S2'.
**
*        preenche_linha 'Q26,0'.
*        preenche_linha 'q810'.
*
*        CONCATENATE 'B30,30,0,3,2,2,50,B,"' wa_mard-matnr '"' INTO wa_linhas.
*        APPEND wa_linhas TO it_linhas.
*
*        CONCATENATE 'A30,120,0,3,1,0,N,"Material:' wa_mard-maktx(15) '"' INTO wa_linhas.
*        APPEND wa_linhas TO it_linhas.
*
*        CONCATENATE 'A30,145,0,3,1,0,N,"' vl_texto(25) '"' INTO wa_linhas.
*        APPEND wa_linhas TO it_linhas.
*
*        SELECT SINGLE name1 INTO centro
*          FROM t001w
*         WHERE werks EQ wa_mard-werks.
*
*        CONCATENATE 'A30,180,0,2,1,0,N,"Centro:' wa_mard-werks '-' centro(13) '"' INTO wa_linhas-linha.
*        APPEND wa_linhas TO it_linhas.
*
*        CONCATENATE 'A30,200,0,2,1,0,N,"Deposito: ' wa_mard-lgort '"' INTO wa_linhas-linha.
*        APPEND wa_linhas TO it_linhas.
*
*        CONCATENATE 'A30,220,0,2,1,0,N,"Endereco: ' wa_mard-lgpbe '"' INTO wa_linhas-linha.
*        APPEND wa_linhas TO it_linhas.
*
*      ELSE.
*        CONCATENATE 'B450,30,0,3,2,2,50,B,"' wa_mard-matnr '"' INTO wa_linhas.
*        APPEND wa_linhas TO it_linhas.
*
*        CONCATENATE 'A450,120,0,3,1,0,N,"Material:' wa_mard-maktx(15) '"' INTO wa_linhas.
*        APPEND wa_linhas TO it_linhas.
*
*        CONCATENATE 'A450,145,0,3,1,0,N,"' vl_texto(25) '"' INTO wa_linhas.
*        APPEND wa_linhas TO it_linhas.
*
*        SELECT SINGLE name1 INTO centro
*          FROM t001w
*         WHERE werks EQ wa_mard-werks.
*
*        CONCATENATE 'A450,180,0,2,1,0,N,"Centro:' wa_mard-werks '-' centro(13) '"' INTO wa_linhas-linha.
*        APPEND wa_linhas TO it_linhas.
*
*        CONCATENATE 'A450,200,0,2,1,0,N,"Deposito: ' wa_mard-lgort '"' INTO wa_linhas-linha.
*        APPEND wa_linhas TO it_linhas.
*
*        CONCATENATE 'A450,220,0,2,1,0,N,"Endereco: ' wa_mard-lgpbe '"' INTO wa_linhas-linha.
*        APPEND wa_linhas TO it_linhas.
*
*        preenche_linha 'P1'.
*      ENDIF.
*    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_mard LINES vl_mod.
  vl_mod = vl_mod MOD 2.

  IF r_col3 = 'X' OR r_col4 = 'X'.
    IF ( vl_mod = 1 ).
      preenche_linha 'P1'.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_GERA_COD_BAR
*&---------------------------------------------------------------------*
*&      Form  F_CHAMA_ETIQUETAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_chama_etiquetas .

*  CLEAR vg_fm_name.
*
*  IF r_col1 EQ 'X'.
***OAY-Rimini-Inicio-16.02.2022-IR084828
  DATA: ls_params TYPE pri_params,
        lv_valid  TYPE c,
        lv_pdest  TYPE sypri_pdest,
        lv_copies TYPE sypri_prcop.

  IF NOT r_usb IS INITIAL.
    lv_copies = 1.
    lv_pdest = 'LOCP'.
    CALL FUNCTION 'GET_PRINT_PARAMETERS'
      EXPORTING
        copies                 = lv_copies
        destination            = lv_pdest
        expiration             = '1'
        immediately            = 'X'
        no_dialog              = `X`
      IMPORTING
        out_parameters         = ls_params
        valid                  = lv_valid
      EXCEPTIONS
        archive_info_not_found = 1
        invalid_print_params   = 2
        invalid_archive_params = 3
        OTHERS                 = 4.

    IF sy-subrc EQ 0.

      ls_params-prnew = space.
      ls_params-primm = 'X'.
      ls_params-prrel = 'X'.
      ls_params-plist = 'ZMMR009'.
      ls_params-prabt = 'ZMM0011'.

      NEW-PAGE PRINT ON PARAMETERS ls_params NO DIALOG NO-HEADING.
      LOOP AT it_linhas INTO wa_linhas.
        WRITE:/ wa_linhas-linha.
      ENDLOOP.
    ENDIF.
  ELSE.
***OAY-Rimini-Fim-16.02.2022-IR084828

    CALL FUNCTION 'WS_DOWNLOAD'
      EXPORTING
        filename                = 'LPT1:'
        filetype                = 'ASC'
      TABLES
        data_tab                = it_linhas
      EXCEPTIONS
        file_open_error         = 1
        file_write_error        = 2
        invalid_filesize        = 3
        invalid_type            = 4
        no_batch                = 5
        unknown_error           = 6
        invalid_table_width     = 7
        gui_refuse_filetransfer = 8
        customer_error          = 9
        no_authority            = 10
        OTHERS                  = 11.
***OAY-Rimini-Inicio-16.02.2022-IR084828
  ENDIF.
***OAY-Rimini-Fim-16.02.2022-IR084828

*        CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
*          EXPORTING
*            FORMNAME = 'ZMMS001_SMART_COL1'
*          IMPORTING
*            FM_NAME  = VG_FM_NAME
*          EXCEPTIONS
*          EXCEPTIONS
*            OTHERS   = 3.
*
*        CHECK SY-SUBRC IS INITIAL.
*
*        CALL FUNCTION VG_FM_NAME
*          TABLES
*            ETI = IT_ETIQUETAS.
*  ELSE.
*    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
*      EXPORTING
*        formname = 'ZMMS001_SMART'
*      IMPORTING
*        fm_name  = vg_fm_name
*      EXCEPTIONS
*        OTHERS   = 3.
*
*    CHECK sy-subrc IS INITIAL.
*
*    CALL FUNCTION vg_fm_name
*      TABLES
*        etiquetas = it_etiquetas.
*  ENDIF.

ENDFORM.                    " F_CHAMA_ETIQUETAS
