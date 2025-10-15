*&---------------------------------------------------------------------*
*& Report  ZGL019
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zgl019 MESSAGE-ID zctb.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: icon,
            slis,
            abap.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: zglt041, zglt043. " ZGLT043.

TYPES: BEGIN OF ty_range_zdep_resp.
TYPES: sign   TYPE tvarv_sign,
       option TYPE tvarv_opti,
       low    TYPE zdep_resp,
       high   TYPE zdep_resp.
TYPES: END OF ty_range_zdep_resp.

TYPES: BEGIN OF ty_range_xubname.
TYPES: sign   TYPE tvarv_sign,
       option TYPE tvarv_opti,
       low    TYPE xubname,
       high   TYPE xubname.
TYPES: END OF ty_range_xubname.

DATA: wa_fil_dep TYPE ty_range_zdep_resp,
      wa_fil_nam TYPE ty_range_xubname.

DATA: it_fil_dep TYPE TABLE OF ty_range_zdep_resp,
      it_fil_nam TYPE TABLE OF ty_range_xubname.

DATA: BEGIN OF tg_zglt041 OCCURS 0.
        INCLUDE STRUCTURE zglt041.
DATA: END OF tg_zglt041.

DATA: BEGIN OF tg_zglt041_aux OCCURS 0.
        INCLUDE STRUCTURE zglt041.
DATA: END OF tg_zglt041_aux.

DATA: BEGIN OF tg_zglt043 OCCURS 0.
        INCLUDE STRUCTURE zglt043.
DATA: END OF tg_zglt043.

DATA: BEGIN OF tg_ska1 OCCURS 0,
        ktopl LIKE ska1-ktopl,
        saknr LIKE ska1-saknr,
        txt50 LIKE skat-txt50,
        ktoks LIKE ska1-ktoks,
      END OF tg_ska1.

DATA: BEGIN OF tg_skb1 OCCURS 0,
        bukrs LIKE skb1-bukrs,
        saknr LIKE skb1-saknr,
        mitkz LIKE skb1-mitkz,
        xspeb LIKE skb1-xspeb,
      END OF tg_skb1.

DATA: BEGIN OF tg_zglt039 OCCURS 0.
        INCLUDE STRUCTURE zglt039.
DATA: END OF tg_zglt039.


TYPES: BEGIN OF ty_mes,
         monat TYPE   zglt043-monat,
       END OF ty_mes.

*DATA: BEGIN OF TG_ZGLT040 OCCURS 0.
*        INCLUDE STRUCTURE ZGLT040.
*DATA: END OF TG_ZGLT040.

DATA: BEGIN OF tg_zimp_cad_depto OCCURS 0.
        INCLUDE STRUCTURE zimp_cad_depto.
DATA: END OF tg_zimp_cad_depto.

" Um para cada Moeda
DATA: it_saldo_1 TYPE TABLE OF zde_fi_gl_saldo_faglflext WITH HEADER LINE,
      it_saldo_2 TYPE TABLE OF zde_fi_gl_saldo_faglflext WITH HEADER LINE,
      it_saldo_3 TYPE TABLE OF zde_fi_gl_saldo_faglflext WITH HEADER LINE,
      it_hints   TYPE TABLE OF alv_s_qinf,
      it_aprova  TYPE TABLE OF zglt059 WITH HEADER LINE,
      it_zglt058 TYPE TABLE OF zglt058 WITH HEADER LINE,
      it_zglt062 TYPE TABLE OF zglt062 WITH HEADER LINE,
      it_zglt063 TYPE TABLE OF zglt063 WITH HEADER LINE.

DATA: it_contas TYPE zct_emp_contas,
      wa_contas TYPE zlc_emp_contas,
      it_mes    TYPE TABLE OF ty_mes.

DATA: BEGIN OF tg_saida OCCURS 0,
        seq              TYPE zglt043-seq,
        search           TYPE iconname,
        status           TYPE iconname,
        status_prazo     TYPE iconname,
        dias_vencido     TYPE i,
        status_aprova    TYPE iconname,
        nivel_aprova     TYPE char3,
        saknr            LIKE skb1-saknr,
        bukrs            TYPE bukrs,
        txt50            LIKE skat-txt50,
        saldo_mi         TYPE fdbl_de_bal_cum, "faglflext-hslvt,
        saldo_mi2        TYPE faglflext-kslvt,
        monat            TYPE zglt043-monat,
        gjahr            TYPE zglt043-gjahr,
        saldo_mi3        TYPE faglflext-oslvt,
        dep_resp         TYPE char30,
        bname            TYPE zglt041-bname2,
        prazo_entr       TYPE datum,
        dt_atualizacao   TYPE zfied023,
        c_balanco        TYPE char30,
        c_nota           TYPE char30,
        cta_monet        TYPE zglt041-cta_monet,
        cta_intercompany TYPE zglt041-cta_intercompany,
        color_cell       TYPE lvc_t_scol,
        tx_fech1         TYPE zfied020,
        usuario_respo    TYPE c LENGTH 200,
        line_color(4)    TYPE c, "Used to store row color attributes
        statusdesc       TYPE char30,
      END OF tg_saida.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.


DATA: xs_events    TYPE slis_alv_event,
      events       TYPE slis_t_event,
      t_print      TYPE slis_print_alv,
      estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      v_report     LIKE sy-repid,
      t_top        TYPE slis_t_listheader,
      lt_sort      TYPE slis_t_sortinfo_alv,
      ls_sort      TYPE slis_sortinfo_alv,
      it_t001      TYPE TABLE OF t001 WITH HEADER LINE.


*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
"DATA   DYFIELDS LIKE DYNPREAD OCCURS 1 WITH HEADER LINE.
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-h01.
  SELECT-OPTIONS: p_bukrs    FOR zglt041-bukrs OBLIGATORY.
  PARAMETERS: p_dep_re TYPE zglt041-dep_resp2,
              p_bname  TYPE zglt041-bname2,
              p_gjahr  TYPE zglt043-gjahr OBLIGATORY.
  SELECT-OPTIONS: p_monat  FOR zglt043-monat OBLIGATORY.
*                P_GJAHR  FOR ZGLT043-GJAHR OBLIGATORY.

*PARAMETERS:P_MONAT  TYPE ZGLT043-MONAT OBLIGATORY,
  PARAMETERS: e_mail  TYPE char01 NO-DISPLAY.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-h02.
  PARAMETERS: p_nao   RADIOBUTTON GROUP g1 DEFAULT 'X',
              p_prazo RADIOBUTTON GROUP g1,
              p_ini   RADIOBUTTON GROUP g1,
              p_lib   RADIOBUTTON GROUP g1,
              p_aprov RADIOBUTTON GROUP g1,
              p_rejei RADIOBUTTON GROUP g1,
              p_all   RADIOBUTTON GROUP g1,
              p_alle  RADIOBUTTON GROUP g1,
              p_inat  RADIOBUTTON GROUP g1.
SELECTION-SCREEN: END OF BLOCK b2.

*USER STORY 163049 - MMSILVA - 08.01.2025 - Inicio
SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-h04.
  PARAMETERS: p_layout TYPE disvariant-variant.
SELECTION-SCREEN: END OF BLOCK b3.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  PERFORM f_alv_variant_f4 CHANGING p_layout.
*USER STORY 163049 - MMSILVA - 08.01.2025 - Fim

*AT SELECTION-SCREEN.                                        "Modificação 08.11.2016
*  AUTHORITY-CHECK OBJECT 'ZFI_BUKRS'                        "Modificação 08.11.2016
*    ID 'BUKRS' FIELD P_BUKRS-LOW.                           "Modificação 08.11.2016
*  IF SY-SUBRC <> 0.                                         "Modificação 08.11.2016
*    SET CURSOR FIELD 'BUKRS-LOW'.                           "Modificação 08.11.2016
*    MESSAGE E091(8B) WITH P_BUKRS-LOW.                      "Modificação 08.11.2016
*  ENDIF.

START-OF-SELECTION.

  PERFORM: seleciona_dados,
           processa_dados,
           monta_alv.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM seleciona_dados .

  DATA: moeda_01          TYPE waers,
        moeda_02          TYPE waers,
        moeda_03          TYPE waers,
        wa_moedas_empresa TYPE x001.

  CLEAR: it_fil_dep, it_fil_nam.

  IF p_dep_re IS NOT INITIAL.
    wa_fil_dep-sign   = 'I'.
    wa_fil_dep-option = 'EQ'.
    wa_fil_dep-low    = p_dep_re.
    wa_fil_dep-high   = p_dep_re.
    APPEND wa_fil_dep TO it_fil_dep.
  ENDIF.

  IF p_bname IS NOT INITIAL.
    wa_fil_nam-sign   = 'I'.
    wa_fil_nam-option = 'EQ'.
    wa_fil_nam-low    = p_bname.
    wa_fil_nam-high   = p_bname.
    APPEND wa_fil_nam TO it_fil_nam.
  ENDIF.

  SELECT * INTO TABLE it_t001
    FROM t001
   WHERE bukrs IN p_bukrs.

  READ TABLE it_t001 INDEX 1.

  CALL FUNCTION 'FI_CURRENCY_INFORMATION'
    EXPORTING
      i_bukrs = it_t001-bukrs
    IMPORTING
      e_x001  = wa_moedas_empresa.

  moeda_01 = it_t001-waers.
  moeda_02 = wa_moedas_empresa-hwae2.
  moeda_03 = wa_moedas_empresa-hwae3.

  LOOP AT it_t001.
    IF moeda_01 NE it_t001-waers.
      MESSAGE s000(z_fi) WITH TEXT-001 it_t001-bukrs TEXT-002 moeda_01. "'Empresa ' 'não utiliza moeda'
      STOP.
    ENDIF.

    CALL FUNCTION 'FI_CURRENCY_INFORMATION'
      EXPORTING
        i_bukrs = it_t001-bukrs
      IMPORTING
        e_x001  = wa_moedas_empresa.

    IF moeda_02 NE wa_moedas_empresa-hwae2.
      MESSAGE s000(z_fi) WITH TEXT-001 it_t001-bukrs TEXT-002 moeda_02. "'Empresa ' 'não utiliza moeda'
    ENDIF.
    IF moeda_03 NE wa_moedas_empresa-hwae3.
      MESSAGE s000(z_fi) WITH TEXT-001 it_t001-bukrs TEXT-002 moeda_03. "'Empresa ' 'não utiliza moeda'
    ENDIF.
  ENDLOOP.

  CLEAR: tg_zglt041.

* 1 - Busca dados ZGLT041
  SELECT r~bukrs
         r~saknr
         r~cod_clas_bal
         r~cod_clas_not2
         r~cod_clas_not
         r~cta_monet
         r~cta_intercompany
         r~dep_resp2
         r~bname2
         r~prazo_entr
         r~crit_vecto
    INTO CORRESPONDING FIELDS OF TABLE tg_zglt041
    FROM zglt041 AS r
   INNER JOIN t001 AS e ON e~bukrs EQ r~bukrs
   WHERE r~bukrs     IN p_bukrs
     AND r~dep_resp2 IN it_fil_dep
     AND r~bname2    IN it_fil_nam
*     AND R~GJAHR     EQ P_GJAHR "/Modificação CS2017000372
     AND EXISTS ( SELECT d~bukrs FROM zglt062 AS d WHERE d~bukrs EQ r~bukrs AND d~dep_resp EQ r~dep_resp2 AND d~bname EQ sy-uname )
     AND NOT EXISTS ( SELECT * FROM zglt063 AS f WHERE f~bukrs EQ r~bukrs AND f~dep_resp EQ r~dep_resp2 AND f~bname EQ sy-uname )
     AND NOT EXISTS ( SELECT * FROM zglt058 AS h WHERE h~bukrs EQ r~bukrs AND h~dep_resp EQ r~dep_resp2 AND h~bname EQ sy-uname )
     AND EXISTS ( SELECT * FROM skb1 AS k WHERE k~bukrs EQ r~bukrs AND k~saknr EQ r~saknr ) "#EC CI_DB_OPERATION_OK[2431747]
     AND EXISTS ( SELECT * FROM ska1 AS t WHERE t~ktopl EQ e~ktopl AND t~saknr EQ r~saknr AND ( ktoks EQ 'YB01' OR ktoks EQ 'YB02' OR ktoks EQ 'YB03' OR ktoks EQ 'YB04' ) ) "---Modificação 15.02.2017 "#EC CI_DB_OPERATION_OK[2431747]
     AND NOT EXISTS ( SELECT * FROM zglt043b AS b WHERE b~ktopl EQ e~ktopl AND b~saknr EQ r~saknr AND b~bukrs EQ r~bukrs ) "/Modificação 11.11.2016/ "#EC CI_DB_OPERATION_OK[2389136]
     AND NOT EXISTS ( SELECT *         "#EC CI_DB_OPERATION_OK[2431747]
                        FROM ska1     AS c "#EC CI_DB_OPERATION_OK[2389136]
                       INNER JOIN zglt043a AS a ON a~ktopl EQ c~ktopl
                                               AND a~ktoks EQ c~ktoks
                       WHERE c~ktopl EQ e~ktopl
                         AND c~saknr EQ r~saknr
                         AND a~bukrs EQ r~bukrs ). "/Modificação 11.11.2016/

  SELECT r~bukrs
         r~saknr
         r~cod_clas_bal
         r~cod_clas_not2
         r~cod_clas_not
         r~cta_monet
         r~cta_intercompany
         r~dep_resp2
         r~bname2
         r~prazo_entr
         r~crit_vecto
    APPENDING CORRESPONDING FIELDS OF TABLE tg_zglt041
    FROM zglt041 AS r
   INNER JOIN t001 AS e ON e~bukrs EQ r~bukrs
   WHERE r~bukrs     IN p_bukrs
     AND r~dep_resp2 IN it_fil_dep
     AND r~bname2    IN it_fil_nam
*     AND R~GJAHR     EQ P_GJAHR "/Modificação CS2017000372
     AND EXISTS ( SELECT * FROM zglt062 AS d WHERE d~bukrs EQ r~bukrs AND d~dep_resp EQ r~dep_resp2 AND d~bname EQ sy-uname )
     AND EXISTS ( SELECT * FROM zglt063 AS f WHERE f~bukrs EQ r~bukrs AND f~dep_resp EQ r~dep_resp2 AND f~bname EQ sy-uname AND f~saknr EQ r~saknr )
     AND NOT EXISTS ( SELECT * FROM zglt058 AS h WHERE h~bukrs EQ r~bukrs AND h~dep_resp EQ r~dep_resp2 AND h~bname EQ sy-uname )
     AND EXISTS ( SELECT * FROM skb1 AS k WHERE k~bukrs EQ r~bukrs AND k~saknr EQ r~saknr ) "#EC CI_DB_OPERATION_OK[2431747]
     AND EXISTS ( SELECT * FROM ska1 AS t WHERE t~ktopl EQ e~ktopl AND t~saknr EQ r~saknr AND ( ktoks EQ 'YB01' OR ktoks EQ 'YB02' OR ktoks EQ 'YB03' OR ktoks EQ 'YB04' ) ) "---Modificação 15.02.2017 "#EC CI_DB_OPERATION_OK[2431747]
     AND NOT EXISTS ( SELECT * FROM zglt043b AS b WHERE b~ktopl EQ e~ktopl AND b~saknr EQ r~saknr AND b~bukrs EQ r~bukrs ) "/Modificação 11.11.2016/ "#EC CI_DB_OPERATION_OK[2389136]
     AND NOT EXISTS ( SELECT *         "#EC CI_DB_OPERATION_OK[2431747]
                        FROM ska1     AS c "#EC CI_DB_OPERATION_OK[2389136]
                       INNER JOIN zglt043a AS a ON a~ktopl EQ c~ktopl
                                               AND a~ktoks EQ c~ktoks
                       WHERE c~ktopl EQ e~ktopl
                         AND c~saknr EQ r~saknr
                         AND a~bukrs EQ r~bukrs ). "/Modificação 11.11.2016/

  SELECT r~bukrs
         r~saknr
         r~cod_clas_bal
         r~cod_clas_not2
         r~cod_clas_not
         r~cta_monet
         r~cta_intercompany
         r~dep_resp2
         r~bname2
         r~prazo_entr
         r~crit_vecto
    APPENDING CORRESPONDING FIELDS OF TABLE tg_zglt041
    FROM zglt041 AS r
   INNER JOIN t001 AS e ON e~bukrs EQ r~bukrs
   WHERE r~bukrs     IN p_bukrs
     AND r~dep_resp2 IN it_fil_dep
     AND r~bname2    IN it_fil_nam
*     AND R~GJAHR     EQ P_GJAHR "/Modificação CS2017000372
     AND EXISTS ( SELECT * FROM zglt058 AS h WHERE h~bukrs EQ r~bukrs AND h~dep_resp EQ r~dep_resp2 AND h~bname EQ sy-uname )
     AND EXISTS ( SELECT * FROM skb1 AS k WHERE k~bukrs EQ r~bukrs AND k~saknr EQ r~saknr ) "#EC CI_DB_OPERATION_OK[2431747]
     AND EXISTS ( SELECT * FROM ska1 AS t WHERE t~ktopl EQ e~ktopl AND t~saknr EQ r~saknr AND ( ktoks EQ 'YB01' OR ktoks EQ 'YB02' OR ktoks EQ 'YB03' OR ktoks EQ 'YB04' ) ) "--Modificação 15.02.2017 "#EC CI_DB_OPERATION_OK[2431747]
     AND NOT EXISTS ( SELECT * FROM zglt043b AS b WHERE b~ktopl EQ e~ktopl AND b~saknr EQ r~saknr AND b~bukrs EQ r~bukrs ) "/Modificação 11.11.2016/ "#EC CI_DB_OPERATION_OK[2389136]
     AND NOT EXISTS ( SELECT *         "#EC CI_DB_OPERATION_OK[2431747]
                        FROM ska1     AS c "#EC CI_DB_OPERATION_OK[2389136]
                       INNER JOIN zglt043a AS a ON a~ktopl EQ c~ktopl
                                               AND a~ktoks EQ c~ktoks
                       WHERE c~ktopl EQ e~ktopl
                         AND c~saknr EQ r~saknr
                         AND a~bukrs EQ r~bukrs ). "/Modificação 11.11.2016/

  IF tg_zglt041[] IS INITIAL.
    MESSAGE s014 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  SORT tg_zglt041 BY bukrs saknr.
  DELETE ADJACENT DUPLICATES FROM tg_zglt041 COMPARING bukrs saknr.

* 2 - Busca de dados Conta Contábil
  SELECT ska1~ktopl ska1~saknr ska1~ktoks "#EC CI_DB_OPERATION_OK[2431747]
         skat~txt50                    "#EC CI_DB_OPERATION_OK[2389136]
    FROM ska1
   INNER JOIN skat ON ( ska1~ktopl = skat~ktopl AND ska1~saknr = skat~saknr )
    INTO CORRESPONDING FIELDS OF TABLE tg_ska1
    FOR ALL ENTRIES IN tg_zglt041
    WHERE ska1~saknr EQ tg_zglt041-saknr
      AND skat~spras EQ sy-langu.

  SELECT bukrs saknr mitkz xspeb       "#EC CI_DB_OPERATION_OK[2431747]
    FROM skb1 INTO TABLE tg_skb1
    FOR ALL ENTRIES IN tg_zglt041
    WHERE bukrs EQ tg_zglt041-bukrs
      AND saknr EQ tg_zglt041-saknr.

* 3 - Busca de dados Classificação de Balanço
  SELECT * FROM zglt039 INTO TABLE tg_zglt039.

* 5 – Busca de Dados Departamento
  SELECT * FROM zimp_cad_depto INTO TABLE tg_zimp_cad_depto.


* 7 – Busca de dados Reconciliação ou
* busca Reconciliações inativas ( que foram modificadas, basicamente são logs )
* a tabela que guarda os logs, possuí a mesma estrutura das tabelas originais,
* alteração feita entre 25-31/2016 por Enio Jesus.

  IF ( p_inat = abap_true ).
    SELECT *
      FROM zglt075
      INTO CORRESPONDING FIELDS OF TABLE tg_zglt043
       FOR ALL ENTRIES IN tg_zglt041
     WHERE bukrs IN p_bukrs
       AND monat IN p_monat   "Alterado 19-10-2022 / Anderson Oenning
       AND gjahr = p_gjahr
       AND saknr = tg_zglt041-saknr.
  ELSE.

*   – Busca de Saldo contábil
    MOVE tg_zglt041[] TO tg_zglt041_aux[].

    LOOP AT tg_zglt041_aux.
      wa_contas-bukrs = tg_zglt041_aux-bukrs.
      wa_contas-saknr = tg_zglt041_aux-saknr.
      APPEND wa_contas TO it_contas.
    ENDLOOP.

    CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
      EXPORTING
        ryear         = p_gjahr
        contas        = it_contas
        p_gerar_todas = 'X'
      TABLES
        it_saldos     = it_saldo_1
        it_saldos_2   = it_saldo_2
        it_saldos_3   = it_saldo_3
      EXCEPTIONS
        moeda_nao_adm = 1
        erro_ledger   = 2
        OTHERS        = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    SELECT *

      FROM zglt043
      INTO TABLE tg_zglt043
       FOR ALL ENTRIES IN tg_zglt041
     WHERE bukrs IN p_bukrs
       AND monat IN p_monat "Alterado 19-10-2022 / Anderson Oenning
       AND gjahr = p_gjahr
       AND saknr = tg_zglt041-saknr.

* 8 – Busca Logs para Mostrar Status Atual.
    SELECT * INTO TABLE it_aprova
      FROM zglt059
       FOR ALL ENTRIES IN tg_zglt041
     WHERE monat IN p_monat   "Alterado 19-10-2022 / Anderson Oenning
       AND gjahr = p_gjahr
       AND bukrs = tg_zglt041-bukrs
       AND saknr = tg_zglt041-saknr
       AND ck_ultimo_log = 'S'.

    SORT it_aprova BY bukrs gjahr monat saknr.

    SELECT * INTO TABLE it_zglt058
      FROM zglt058
       FOR ALL ENTRIES IN tg_zglt041
     WHERE bukrs    EQ tg_zglt041-bukrs
       AND dep_resp EQ tg_zglt041-dep_resp2.

    SORT it_zglt058 BY bukrs dep_resp.

    SELECT * INTO TABLE it_zglt062
      FROM zglt062
       FOR ALL ENTRIES IN tg_zglt041
     WHERE bukrs    EQ tg_zglt041-bukrs
       AND dep_resp EQ tg_zglt041-dep_resp2.

    SORT it_zglt062 BY bukrs dep_resp.

    SELECT * INTO TABLE it_zglt063
      FROM zglt063
       FOR ALL ENTRIES IN tg_zglt041
     WHERE bukrs    EQ tg_zglt041-bukrs
       AND dep_resp EQ tg_zglt041-dep_resp2.

    SORT it_zglt063 BY bukrs dep_resp saknr.
  ENDIF.
ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  PROCESSA_DADOS
*&---------------------------------------------------------------------*
FORM processa_dados .

  CLEAR: tg_saida.
  REFRESH tg_saida.

  SORT: tg_zglt043        BY bukrs saknr,
        tg_ska1           BY saknr,
        tg_zimp_cad_depto BY dep_resp,
        tg_zglt039        BY codigo cod_nota,
        tg_zimp_cad_depto BY dep_resp,
        it_saldo_1        BY rbukrs racct,
        it_saldo_2        BY rbukrs racct,
        it_saldo_3        BY rbukrs racct.

  DATA: wl_tabix        TYPE sy-tabix,
        refe1           TYPE hslvt12,
        vg_mes_pos      TYPE i,
        it_contas       TYPE zct_emp_contas,
        it_saldos_atual TYPE TABLE OF zde_fi_gl_saldo_faglflext WITH HEADER LINE,
        vg_saldo_mi     TYPE hslvt12,
        p_waers         TYPE waers,
        it_zglt043b     TYPE TABLE OF zglt043b WITH HEADER LINE,
        it_zglt043a     TYPE TABLE OF zglt043a WITH HEADER LINE,
        it_ska1         TYPE TABLE OF ska1 WITH HEADER LINE,
        wa_col          TYPE lvc_s_scol,
        lc_monat        TYPE zglt043-monat,
        lv_saknr        TYPE zglt043-saknr,
        lc_gjahr        TYPE zglt043-gjahr,
        lc_value        TYPE p DECIMALS 4,
*---> 20.06.2023 - Migração S4 - DG
        "        vg_lines_mes    TYPE char02,
        vg_lines_mes    TYPE zchar02,
*<--- 20.06.2023 - Migração S4 - DG
*---> 20.06.2023 - Migração S4 - DG
        "        vg_mes          TYPE char02,
        vg_mes          TYPE zchar02,
*<--- 20.06.2023 - Migração S4 - DG
        vg_nivel        TYPE zglt058-nivel,
        vg_found        TYPE flag. "2000004155 - IR172865


  "Verifica se tem mais de 1 mes selecionado, se sim, fazer o processo para cada més.
  DATA(vg_dat) = p_monat-low.

  IF p_monat-high IS NOT INITIAL.
    CLEAR: vg_mes.
    WHILE vg_mes < p_monat-high.
      ADD 1 TO vg_mes.

      IF vg_mes < 10.
        APPEND VALUE #( low = |0{ vg_mes }| sign = 'I' option = 'EQ' ) TO p_monat.
      ELSE.
        APPEND VALUE #( low = vg_mes sign = 'I' option = 'EQ' ) TO p_monat.
      ENDIF.
    ENDWHILE.

    DELETE p_monat WHERE high NE space.
    SORT p_monat BY low .
  ENDIF.


  IF p_monat[] IS NOT INITIAL.
    DELETE ADJACENT DUPLICATES FROM p_monat COMPARING low.
    DELETE p_monat WHERE low < vg_dat.
  ENDIF.

  CLEAR: vg_dat.

  LOOP AT p_monat.


    IF p_alle IS NOT INITIAL.

      CLEAR: it_contas.
      CLEAR: it_saldos_atual[], tg_zglt041_aux[].
      LOOP AT it_t001.
        wa_contas-bukrs = it_t001-bukrs.
        wa_contas-saknr = '*'.
        APPEND wa_contas TO it_contas.
      ENDLOOP.

      p_waers = 'BRL'.

      CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
        EXPORTING
          ryear         = p_gjahr
          waers         = p_waers
          contas        = it_contas
        TABLES
          it_saldos     = it_saldos_atual
        EXCEPTIONS
          moeda_nao_adm = 1
          erro_ledger   = 2
          OTHERS        = 3.

      vg_mes_pos = p_monat-low.

      IF vg_mes_pos EQ 12.
        vg_mes_pos = 16.
      ENDIF.

      SELECT * INTO TABLE it_zglt043b
        FROM zglt043b
         FOR ALL ENTRIES IN it_t001
       WHERE ktopl EQ it_t001-ktopl
         AND bukrs EQ it_t001-bukrs. "/Modificação 11.11.2016/

      SORT it_zglt043b BY saknr.

      SELECT * INTO TABLE it_zglt043a
        FROM zglt043a
         FOR ALL ENTRIES IN it_t001
       WHERE ktopl EQ it_t001-ktopl
         AND bukrs EQ it_t001-bukrs. "/Modificação 11.11.2016/

      SORT it_zglt043a BY ktoks.

      SELECT * INTO TABLE it_ska1      "#EC CI_DB_OPERATION_OK[2431747]
        FROM ska1                      "#EC CI_DB_OPERATION_OK[2389136]
         FOR ALL ENTRIES IN it_t001
       WHERE ktopl EQ it_t001-ktopl.

      SORT it_ska1 BY saknr.

      LOOP AT it_saldos_atual.

        READ TABLE it_zglt043b WITH KEY saknr = it_saldos_atual-racct BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          CONTINUE.
        ENDIF.

        READ TABLE it_ska1 WITH KEY saknr = it_saldos_atual-racct BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          READ TABLE it_zglt043a WITH KEY ktoks = it_ska1-ktoks BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            CONTINUE.
          ENDIF.
        ENDIF.

        vg_saldo_mi = it_saldos_atual-slvt.
        DO vg_mes_pos TIMES
          VARYING refe1 FROM it_saldos_atual-sl01 NEXT it_saldos_atual-sl02.
          ADD refe1 TO vg_saldo_mi.
        ENDDO.

        IF vg_saldo_mi NE 0.
          READ TABLE tg_zglt041 WITH KEY bukrs = it_saldos_atual-rbukrs
                                         saknr = it_saldos_atual-racct.
          IF sy-subrc IS NOT INITIAL.
            tg_zglt041_aux-bukrs = it_saldos_atual-rbukrs.
            tg_zglt041_aux-saknr = it_saldos_atual-racct.
            APPEND tg_zglt041_aux.
          ENDIF.
        ENDIF.

      ENDLOOP.

      CLEAR: tg_zglt041[].
      MOVE tg_zglt041_aux[] TO tg_zglt041[].

      IF tg_zglt041[] IS NOT INITIAL.

*     2 - Busca de dados Conta Contábil
        SELECT ska1~ktopl ska1~saknr ska1~ktoks "#EC CI_DB_OPERATION_OK[2389136]
               skat~txt50              "#EC CI_DB_OPERATION_OK[2431747]
          FROM ska1 INNER JOIN skat ON ( ska1~ktopl = skat~ktopl AND ska1~saknr = skat~saknr )
          APPENDING CORRESPONDING FIELDS OF TABLE tg_ska1
          FOR ALL ENTRIES IN tg_zglt041
          WHERE ska1~saknr EQ tg_zglt041-saknr
            AND skat~spras EQ sy-langu.

        SELECT bukrs saknr mitkz xspeb "#EC CI_DB_OPERATION_OK[2431747]
          FROM skb1 APPENDING TABLE tg_skb1
          FOR ALL ENTRIES IN tg_zglt041
          WHERE bukrs EQ tg_zglt041-bukrs
            AND saknr EQ tg_zglt041-saknr.

      ENDIF.

    ENDIF.


    IF ( p_inat = abap_true ).
      LOOP AT tg_zglt043.

        SORT: tg_ska1 BY ktopl saknr,
              it_t001 BY bukrs.

        CLEAR: tg_saida.
        READ TABLE tg_zglt041 WITH KEY bukrs = tg_zglt043-bukrs
                                       saknr = tg_zglt043-saknr.

        SELECT SINGLE responsavel
          FROM zglt075
          INTO tg_saida-bname
         WHERE bukrs = tg_zglt043-bukrs
           AND saknr = tg_zglt043-saknr
           AND monat = tg_zglt043-monat
           AND gjahr = tg_zglt043-gjahr.

        tg_saida-seq       = tg_zglt043-seq.
        tg_saida-bukrs     = tg_zglt043-bukrs.
        tg_saida-tx_fech1  = tg_zglt043-tx_fech1.
        tg_saida-search    = icon_display.
        tg_saida-status    = icon_led_inactive.
        tg_saida-gjahr     = tg_zglt043-gjahr.
        tg_saida-monat     = tg_zglt043-monat.
        tg_saida-dt_atualizacao = tg_zglt043-dt_atual.
        tg_saida-saldo_mi       = tg_zglt043-sdo_mi.
        tg_saida-saldo_mi2      = tg_zglt043-sdo_mi2.
*        LC_MONAT = P_MONAT.
*        LC_GJAHR = P_GJAHR.
*
*        IF LC_MONAT EQ 12.
*          ADD 1 TO LC_GJAHR.
*          LC_MONAT = 1.
*        ELSE.
*          ADD 1 TO LC_MONAT.
*        ENDIF.
*
*        CONCATENATE LC_GJAHR LC_MONAT '01' INTO TG_SAIDA-PRAZO_ENTR.
*        SUBTRACT 1 FROM TG_SAIDA-PRAZO_ENTR. "obtem ultimo dia do mês.
*
*        ADD ZGLT041-PRAZO_ENTR TO TG_SAIDA-PRAZO_ENTR. "adiciona prazo.
*
*        "Não Vencido
*        TG_SAIDA-STATUS_PRAZO = ICON_RESUBMISSION.
*        TG_SAIDA-DIAS_VENCIDO = 0.
*        IF ( SY-DATUM GE TG_SAIDA-PRAZO_ENTR ) AND ( TG_ZGLT043-STATUS_LIB EQ 'P' OR TG_ZGLT043-STATUS_LIB EQ ' ' OR TG_ZGLT043-STATUS_LIB EQ 'L' ). "Verifica se está vencido
*
*          "Vencido
*          IF SY-DATUM EQ TG_SAIDA-PRAZO_ENTR.
*            TG_SAIDA-STATUS_PRAZO = ICON_ALARM.
*            "Em Atraso
*          ELSEIF SY-DATUM GT TG_SAIDA-PRAZO_ENTR.
*            TG_SAIDA-STATUS_PRAZO = ICON_ALERT.
*          ENDIF.
*
*          TG_SAIDA-DIAS_VENCIDO = TG_SAIDA-PRAZO_ENTR - SY-DATUM.
*          TG_SAIDA-DIAS_VENCIDO = ABS( TG_SAIDA-DIAS_VENCIDO ).
*        ENDIF.

        READ TABLE it_t001 WITH KEY bukrs = tg_zglt041-bukrs BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          TRY.
              tg_ska1 = tg_ska1[ ktopl = it_t001-ktopl saknr = tg_zglt041-saknr ].
              MOVE: tg_ska1-saknr TO tg_saida-saknr,
                    tg_ska1-txt50 TO tg_saida-txt50.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.
        ENDIF.
*
*        MOVE: TG_ZGLT041-BNAME2           TO TG_SAIDA-BNAME,
*              TG_ZGLT041-CTA_MONET        TO TG_SAIDA-CTA_MONET,
*              TG_ZGLT041-CTA_INTERCOMPANY TO TG_SAIDA-CTA_INTERCOMPANY.

*       Monta descrições
        READ TABLE tg_zglt039 WITH KEY codigo   = tg_zglt041-cod_clas_bal
                                       cod_nota = tg_zglt041-cod_clas_not2
                                       BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          CONCATENATE tg_zglt041-cod_clas_bal tg_zglt039-descr      INTO tg_saida-c_balanco SEPARATED BY ' - '.
          CONCATENATE tg_zglt039-cod_nota     tg_zglt039-descr_nota INTO tg_saida-c_nota    SEPARATED BY ' - '.
        ENDIF.

        READ TABLE tg_zimp_cad_depto WITH KEY dep_resp = tg_zglt041-dep_resp2 BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          CONCATENATE tg_zglt041-dep_resp2 tg_zimp_cad_depto-dep_resp_desc INTO tg_saida-dep_resp SEPARATED BY ' - '.
        ENDIF.

*        VG_MES_POS = P_MONAT.
*
*        IF VG_MES_POS EQ 12.
*          VG_MES_POS = 16.
*        ENDIF.
*
*        READ TABLE IT_SALDO_1 WITH KEY RBUKRS = TG_ZGLT041-BUKRS
*                                       RACCT  = TG_ZGLT041-SAKNR
*                                       BINARY SEARCH.
*        IF SY-SUBRC IS INITIAL.
*          TG_SAIDA-SALDO_MI = IT_SALDO_1-SLVT.
*          DO VG_MES_POS TIMES
*            VARYING REFE1 FROM IT_SALDO_1-SL01 NEXT IT_SALDO_1-SL02.
*            ADD REFE1 TO TG_SAIDA-SALDO_MI.
*          ENDDO.
*        ENDIF.
*
*        READ TABLE IT_SALDO_2 WITH KEY RBUKRS = TG_ZGLT041-BUKRS
*                                       RACCT  = TG_ZGLT041-SAKNR
*                                       BINARY SEARCH.
*        IF SY-SUBRC IS INITIAL.
*          TG_SAIDA-SALDO_MI2 = IT_SALDO_2-SLVT.
*          DO VG_MES_POS TIMES
*            VARYING REFE1 FROM IT_SALDO_2-SL01 NEXT IT_SALDO_2-SL02.
*            ADD REFE1 TO TG_SAIDA-SALDO_MI2.
*          ENDDO.
*        ENDIF.
*
*        "BREAK-POINT.
*
*        IF TG_SAIDA-TX_FECH1 IS INITIAL.
*          IF TG_SAIDA-SALDO_MI2 NE 0.
*
*            LC_VALUE = ( TG_SAIDA-SALDO_MI / TG_SAIDA-SALDO_MI2 ).
*
*            IF LC_VALUE GT 9999999.
*              TG_SAIDA-TX_FECH1 = 9999999.
*            ELSE.
*              MOVE LC_VALUE TO TG_SAIDA-TX_FECH1.
*              TG_SAIDA-TX_FECH1 = ABS( TG_SAIDA-TX_FECH1 ).
*            ENDIF.
*          ELSE.
*            TG_SAIDA-TX_FECH1 = 0.
*          ENDIF.
*        ENDIF.
*
*        READ TABLE IT_SALDO_3 WITH KEY RBUKRS = TG_ZGLT041-BUKRS
*                                       RACCT  = TG_ZGLT041-SAKNR
*                                       BINARY SEARCH.
*        IF SY-SUBRC IS INITIAL.
*          TG_SAIDA-SALDO_MI3 = IT_SALDO_3-SLVT.
*          DO VG_MES_POS TIMES
*            VARYING REFE1 FROM IT_SALDO_3-SL01 NEXT IT_SALDO_3-SL02.
*            ADD REFE1 TO TG_SAIDA-SALDO_MI3.
*          ENDDO.
*        ENDIF.

        IF tg_saida-monat IS NOT INITIAL.
          tg_saida-monat = p_monat-low.
        ENDIF.

        APPEND tg_saida.
        CLEAR: tg_saida , tg_zglt043, tg_zglt041 , tg_zglt039 , it_aprova, vg_nivel, it_t001 ,it_saldo_2 ,it_saldo_1, tg_zimp_cad_depto, it_saldo_3.
      ENDLOOP.

    ELSE.
      LOOP AT tg_zglt041.

        READ TABLE tg_zglt043 WITH KEY bukrs = tg_zglt041-bukrs
                                       saknr = tg_zglt041-saknr
                                       monat = p_monat-low.
        "BINARY SEARCH.
        CLEAR vg_found.
        IF sy-subrc EQ 0.        "2000004155 - IR172865
          vg_found = abap_true.  "2000004155 - IR172865
        ENDIF.                   "2000004155 - IR172865

        tg_saida-bukrs = tg_zglt041-bukrs.
        tg_saida-tx_fech1 = tg_zglt043-tx_fech1.
* Validação para Vencimento
        tg_saida-dt_atualizacao = tg_zglt043-dt_atual.


        lc_monat = p_monat-low.
        lc_gjahr = p_gjahr.

        IF lc_monat EQ 12.
          ADD 1 TO lc_gjahr.
          lc_monat = 1.
        ELSE.
          ADD 1 TO lc_monat.
        ENDIF.

        CONCATENATE lc_gjahr lc_monat '01' INTO tg_saida-prazo_entr.
        SUBTRACT 1 FROM tg_saida-prazo_entr. "obtem ultimo dia do mês.

        ADD zglt041-prazo_entr TO tg_saida-prazo_entr. "adiciona prazo.

        "Não Vencido
        tg_saida-status_prazo = icon_resubmission.
        tg_saida-dias_vencido = 0.
        IF ( sy-datum GE tg_saida-prazo_entr ) AND ( tg_zglt043-status_lib EQ 'P' OR tg_zglt043-status_lib EQ ' ' OR tg_zglt043-status_lib EQ 'L' ). "Verifica se está vencido

          "Vencido
          IF sy-datum EQ tg_saida-prazo_entr.
            tg_saida-status_prazo = icon_alarm.
            "Em Atraso
          ELSEIF sy-datum GT tg_saida-prazo_entr.
            tg_saida-status_prazo = icon_alert.
          ENDIF.

          tg_saida-dias_vencido = tg_saida-prazo_entr - sy-datum.
          tg_saida-dias_vencido = abs( tg_saida-dias_vencido ).

        ENDIF.

        CASE 'X'. "Status

          WHEN p_nao. "Não Iniciado
            IF tg_saida-status NE icon_light_out.
              CONTINUE.
            ENDIF.

          WHEN p_prazo. "Prazo de Entrega Vencido
            IF ( tg_saida-status_prazo NE icon_alert ) OR ( tg_saida-status EQ icon_release ).
              CONTINUE.
            ENDIF.

          WHEN p_ini. "Iniciado
            IF tg_saida-status NE icon_yellow_light.
              CONTINUE.
            ENDIF.

          WHEN p_lib. "Liberado
            IF tg_saida-status NE icon_green_light.
              CONTINUE.
            ENDIF.

          WHEN p_aprov. "Aprovado
            IF tg_saida-status NE icon_release.
              CONTINUE.
            ENDIF.

          WHEN p_rejei. "Rejeitado
            IF tg_saida-status NE icon_defect.
              CONTINUE.
            ENDIF.

        ENDCASE.

        READ TABLE it_t001 WITH KEY bukrs = tg_zglt041-bukrs BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          TRY.
              tg_ska1 = tg_ska1[ ktopl = it_t001-ktopl saknr = tg_zglt041-saknr ].
              MOVE: tg_ska1-saknr TO tg_saida-saknr,
                    tg_ska1-txt50 TO tg_saida-txt50.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.
        ENDIF.

* Move ZGLT041
        MOVE: tg_zglt041-bname2           TO tg_saida-bname,
              tg_zglt041-cta_monet        TO tg_saida-cta_monet,
              tg_zglt041-cta_intercompany TO tg_saida-cta_intercompany.

* Monta descrições
        READ TABLE tg_zglt039 WITH KEY codigo   = tg_zglt041-cod_clas_bal
                                       cod_nota = tg_zglt041-cod_clas_not2
                                       BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          CONCATENATE tg_zglt041-cod_clas_bal tg_zglt039-descr      INTO tg_saida-c_balanco SEPARATED BY ' - '.
          CONCATENATE tg_zglt039-cod_nota     tg_zglt039-descr_nota INTO tg_saida-c_nota    SEPARATED BY ' - '.
        ENDIF.

        READ TABLE tg_zimp_cad_depto WITH KEY dep_resp = tg_zglt041-dep_resp2 BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          CONCATENATE tg_zglt041-dep_resp2 tg_zimp_cad_depto-dep_resp_desc INTO tg_saida-dep_resp SEPARATED BY ' - '.
        ENDIF.

        vg_mes_pos = p_monat-low.

        IF vg_mes_pos EQ 12.
          vg_mes_pos = 16.
        ENDIF.

        READ TABLE it_saldo_1 WITH KEY rbukrs = tg_zglt041-bukrs
                                       racct  = tg_zglt041-saknr
                                       BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          tg_saida-saldo_mi = it_saldo_1-slvt.
          DO vg_mes_pos TIMES
            VARYING refe1 FROM it_saldo_1-sl01 NEXT it_saldo_1-sl02.
            ADD refe1 TO tg_saida-saldo_mi.
          ENDDO.
        ENDIF.

        READ TABLE it_saldo_2 WITH KEY rbukrs = tg_zglt041-bukrs
                                       racct  = tg_zglt041-saknr
                                       BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          tg_saida-saldo_mi2 = it_saldo_2-slvt.

          IF vg_mes_pos => 1.
            tg_saida-saldo_mi2 = tg_saida-saldo_mi2 + it_saldo_2-sl01.
          ENDIF.
          IF vg_mes_pos => 2.
            tg_saida-saldo_mi2 = tg_saida-saldo_mi2 + it_saldo_2-sl02.
          ENDIF.
          IF vg_mes_pos => 3.
            tg_saida-saldo_mi2 = tg_saida-saldo_mi2 + it_saldo_2-sl03.
          ENDIF.
          IF vg_mes_pos => 4.
            tg_saida-saldo_mi2 = tg_saida-saldo_mi2 + it_saldo_2-sl04.
          ENDIF.
          IF vg_mes_pos => 5.
            tg_saida-saldo_mi2 = tg_saida-saldo_mi2 + it_saldo_2-sl05.
          ENDIF.
          IF vg_mes_pos => 6.
            tg_saida-saldo_mi2 = tg_saida-saldo_mi2 + it_saldo_2-sl06.
          ENDIF.
          IF vg_mes_pos => 7.
            tg_saida-saldo_mi2 = tg_saida-saldo_mi2 + it_saldo_2-sl07.
          ENDIF.
          IF vg_mes_pos => 8.
            tg_saida-saldo_mi2 = tg_saida-saldo_mi2 + it_saldo_2-sl08.
          ENDIF.
          IF vg_mes_pos => 9.
            tg_saida-saldo_mi2 = tg_saida-saldo_mi2 + it_saldo_2-sl09.
          ENDIF.
          IF vg_mes_pos => 10.
            tg_saida-saldo_mi2 = tg_saida-saldo_mi2 + it_saldo_2-sl10.
          ENDIF.
          IF vg_mes_pos => 11.
            tg_saida-saldo_mi2 = tg_saida-saldo_mi2 + it_saldo_2-sl11.
          ENDIF.
          IF vg_mes_pos => 12.
            tg_saida-saldo_mi2 = tg_saida-saldo_mi2 + it_saldo_2-sl12.
          ENDIF.
          IF vg_mes_pos => 13.
            tg_saida-saldo_mi2 = tg_saida-saldo_mi2 + it_saldo_2-sl13.
          ENDIF.
          IF vg_mes_pos => 14.
            tg_saida-saldo_mi2 = tg_saida-saldo_mi2 + it_saldo_2-sl14.
          ENDIF.
          IF vg_mes_pos => 15.
            tg_saida-saldo_mi2 = tg_saida-saldo_mi2 + it_saldo_2-sl15.
          ENDIF.
          IF vg_mes_pos => 16.
            tg_saida-saldo_mi2 = tg_saida-saldo_mi2 + it_saldo_2-sl16.
          ENDIF.

*          DO vg_mes_pos TIMES
*            VARYING refe1 FROM it_saldo_2-sl01 NEXT it_saldo_2-sl02.
*            ADD refe1 TO tg_saida-saldo_mi2.
*          ENDDO.
        ENDIF.

        "BREAK-POINT.

        IF tg_saida-tx_fech1 IS INITIAL.
          IF tg_saida-saldo_mi2 NE 0.

            lc_value = ( tg_saida-saldo_mi / tg_saida-saldo_mi2 ).

*        CALL FUNCTION 'ROUND'
*          EXPORTING
*            DECIMALS      = 4
*            INPUT         = LC_VALUE
*            SIGN          = 'X'
*          IMPORTING
*            OUTPUT        = TG_SAIDA-TX_FECH1
*          EXCEPTIONS
*            INPUT_INVALID = 1
*            OVERFLOW      = 2
*            TYPE_INVALID  = 3
*            OTHERS        = 4.
*
*        IF SY-SUBRC <> 0.
*          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*        ENDIF.
            IF lc_value GT 9999999.
              tg_saida-tx_fech1 = 9999999.
            ELSE.
              MOVE lc_value TO tg_saida-tx_fech1.
              tg_saida-tx_fech1 = abs( tg_saida-tx_fech1 ).
            ENDIF.
          ELSE.
            tg_saida-tx_fech1 = 0.
          ENDIF.
        ENDIF.

        READ TABLE it_saldo_3 WITH KEY rbukrs = tg_zglt041-bukrs
                                       racct  = tg_zglt041-saknr
                                       BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          tg_saida-saldo_mi3 = it_saldo_3-slvt.

          IF vg_mes_pos => 1.
            tg_saida-saldo_mi3 = tg_saida-saldo_mi3 + it_saldo_3-sl01.
          ENDIF.
          IF vg_mes_pos => 2.
            tg_saida-saldo_mi3 = tg_saida-saldo_mi3 + it_saldo_3-sl02.
          ENDIF.
          IF vg_mes_pos => 3.
            tg_saida-saldo_mi3 = tg_saida-saldo_mi3 + it_saldo_3-sl03.
          ENDIF.
          IF vg_mes_pos => 4.
            tg_saida-saldo_mi3 = tg_saida-saldo_mi3 + it_saldo_3-sl04.
          ENDIF.
          IF vg_mes_pos => 5.
            tg_saida-saldo_mi3 = tg_saida-saldo_mi3 + it_saldo_3-sl05.
          ENDIF.
          IF vg_mes_pos => 6.
            tg_saida-saldo_mi3 = tg_saida-saldo_mi3 + it_saldo_3-sl06.
          ENDIF.
          IF vg_mes_pos => 7.
            tg_saida-saldo_mi3 = tg_saida-saldo_mi3 + it_saldo_3-sl07.
          ENDIF.
          IF vg_mes_pos => 8.
            tg_saida-saldo_mi3 = tg_saida-saldo_mi3 + it_saldo_3-sl08.
          ENDIF.
          IF vg_mes_pos => 9.
            tg_saida-saldo_mi3 = tg_saida-saldo_mi3 + it_saldo_3-sl09.
          ENDIF.
          IF vg_mes_pos => 10.
            tg_saida-saldo_mi3 = tg_saida-saldo_mi3 + it_saldo_3-sl10.
          ENDIF.
          IF vg_mes_pos => 11.
            tg_saida-saldo_mi3 = tg_saida-saldo_mi3 + it_saldo_3-sl11.
          ENDIF.
          IF vg_mes_pos => 12.
            tg_saida-saldo_mi3 = tg_saida-saldo_mi3 + it_saldo_3-sl12.
          ENDIF.
          IF vg_mes_pos => 13.
            tg_saida-saldo_mi3 = tg_saida-saldo_mi3 + it_saldo_3-sl13.
          ENDIF.
          IF vg_mes_pos => 14.
            tg_saida-saldo_mi3 = tg_saida-saldo_mi3 + it_saldo_3-sl14.
          ENDIF.
          IF vg_mes_pos => 15.
            tg_saida-saldo_mi3 = tg_saida-saldo_mi3 + it_saldo_3-sl15.
          ENDIF.
          IF vg_mes_pos => 16.
            tg_saida-saldo_mi3 = tg_saida-saldo_mi3 + it_saldo_3-sl16.
          ENDIF.
*          DO vg_mes_pos TIMES
*            VARYING refe1 FROM it_saldo_3-sl01 NEXT it_saldo_3-sl02.
*            ADD refe1 TO tg_saida-saldo_mi3.
*          ENDDO.
        ENDIF.

        READ TABLE it_aprova WITH KEY bukrs = tg_saida-bukrs
                                      gjahr = p_gjahr
                                      monat = p_monat-low
                                      saknr = tg_saida-saknr.
        "BINARY SEARCH.
        "Entrou na Estratégia de Aprovação
        IF ( sy-subrc IS INITIAL ).

*      IF ( TG_SAIDA-STATUS EQ ICON_RELEASE ).
*        TG_SAIDA-STATUS_APROVA = ICON_RELEASE.
*        TG_SAIDA-USUARIO_RESPO = IT_APROVA-BN_LIBERACAO.
*        VG_NIVEL               = IT_APROVA-NIVEL.
*      ELSE.

          CASE it_aprova-status_lib.
              "//Aprovado;
            WHEN 'A'.
              tg_saida-status_aprova = icon_release.
              tg_saida-usuario_respo = it_aprova-bn_liberacao.
              vg_nivel               = it_aprova-nivel.

              "//Liberado para o próximo nível;
            WHEN 'L'.

              IF ( it_aprova-nivel IS INITIAL ).
                tg_saida-status_aprova = icon_led_yellow.
                vg_nivel               = 1.
              ELSE.

                tg_saida-status_aprova = icon_led_green.
                TRY.
                    it_zglt058 = it_zglt058[ bukrs    = tg_zglt041-bukrs
                                             dep_resp = tg_zglt041-dep_resp2
                                             nivel    = it_aprova-nivel + 1 ].

                    vg_nivel = it_aprova-nivel + 1.

                  CATCH cx_sy_itab_line_not_found.
                    vg_nivel = it_aprova-nivel.
                ENDTRY.

              ENDIF.

              LOOP AT it_zglt058 WHERE bukrs    EQ tg_zglt041-bukrs
                                   AND dep_resp EQ tg_zglt041-dep_resp2
                                   AND nivel    EQ vg_nivel.

                IF ( tg_saida-usuario_respo IS INITIAL ).
                  tg_saida-usuario_respo = it_zglt058-bname.
                ELSE.
                  CONCATENATE tg_saida-usuario_respo ',' INTO tg_saida-usuario_respo.
                  CONCATENATE tg_saida-usuario_respo it_zglt058-bname INTO tg_saida-usuario_respo SEPARATED BY space.
                ENDIF.
              ENDLOOP.

              "//Rejeitado;
            WHEN 'R'.
              tg_saida-status_aprova = icon_led_red.

              IF ( it_aprova-nivel EQ 1 ). "Retornado para reconciliação
                vg_nivel = 000.
                PERFORM busca_usuarios_reconciliacao USING
                                                     tg_zglt041-bukrs
                                                     tg_zglt041-dep_resp2
                                                     tg_zglt041-saknr
                                                     CHANGING
                                                     tg_saida-usuario_respo.
              ELSE.

                vg_nivel = it_aprova-nivel - 1.
                LOOP AT it_zglt058 WHERE bukrs    EQ tg_zglt041-bukrs
                                     AND dep_resp EQ tg_zglt041-dep_resp2
                                     AND nivel    EQ vg_nivel.

                  IF ( tg_saida-usuario_respo IS INITIAL ).
                    tg_saida-usuario_respo = it_zglt058-bname.
                  ELSE.
                    CONCATENATE tg_saida-usuario_respo ',' INTO tg_saida-usuario_respo.
                    CONCATENATE tg_saida-usuario_respo it_zglt058-bname INTO tg_saida-usuario_respo SEPARATED BY space.
                  ENDIF.
                ENDLOOP.
              ENDIF.
          ENDCASE.

        ELSE.
          "//Status inicial;
          tg_saida-status_aprova = icon_led_inactive.
          vg_nivel               = 000.

          PERFORM busca_usuarios_reconciliacao USING
                                               tg_zglt041-bukrs
                                               tg_zglt041-dep_resp2
                                               tg_zglt041-saknr
                                               CHANGING
                                               tg_saida-usuario_respo.
        ENDIF.

        IF ( vg_nivel IS NOT INITIAL ).
          SHIFT vg_nivel LEFT DELETING LEADING '0'.
          tg_saida-nivel_aprova = vg_nivel.
        ELSE.
          tg_saida-nivel_aprova = vg_nivel(1).
        ENDIF.
        tg_saida-monat = p_monat-low.



        IF tg_zglt043-status_lib IS NOT INITIAL.


          CASE tg_zglt043-status_lib.
            WHEN 'S'.
              tg_saida-status = icon_booking_ok.
            WHEN ' '.
              tg_saida-status = icon_light_out.
            WHEN 'P'.
              tg_saida-status = icon_yellow_light.
            WHEN 'L'.
              tg_saida-status = icon_green_light.
            WHEN 'A'.
              tg_saida-status = icon_release.
            WHEN 'R'.
              tg_saida-status = icon_defect.

          ENDCASE.


          CASE tg_zglt043-status_lib.
            WHEN 'S'.
              tg_saida-statusdesc = 'Sem Movimento'.
            WHEN ' '.
              tg_saida-statusdesc = 'Não Iniciado'.
            WHEN 'P'.
              tg_saida-statusdesc = 'Aguardando Liberação'.
            WHEN 'L'.
              tg_saida-statusdesc = 'Liberado'.
            WHEN 'A'.
              tg_saida-statusdesc = 'Aprovado'.
            WHEN 'R'.
              tg_saida-statusdesc = 'Reprovado'.
          ENDCASE.

        ELSE.
          "CLEAR: tg_zglt043.
          tg_saida-status = icon_light_out.
          tg_saida-statusdesc = 'Não Iniciado'.

        ENDIF.

******************************************************************************************************
        DATA: v_mes    TYPE zglt042-mes_de,
              v_ano    TYPE zglt042-ano_de,
              v_dt(07) TYPE c.

        v_mes = p_monat-low.
        v_ano = p_gjahr.

        CONCATENATE v_mes '.' v_ano INTO v_dt.
        CONCATENATE v_ano v_mes  INTO v_dt.

        SELECT * FROM tvarvc
          INTO @DATA(ls_tvarvc)
          WHERE name = 'ZGL026_DT_CORTE'
            AND low <= @v_dt.
        ENDSELECT.

        IF sy-subrc = 0.

          IF v_mes = '1'.

            v_ano = v_ano - 1.
            v_mes = '12'.

          ELSE.

            v_mes = v_mes - 1.

          ENDIF.

          SELECT * FROM zglt043
            INTO @DATA(ls_check)
            WHERE monat = @v_mes
             AND  gjahr = @v_ano
             AND  bukrs = @tg_saida-bukrs
             AND  saknr = @tg_saida-saknr.
          ENDSELECT.

          IF sy-subrc = 0.
            IF ls_check-sdo_mi   = tg_saida-saldo_mi  AND ls_check-sdo_mi2   = tg_saida-saldo_mi2.

              tg_saida-status = icon_booking_ok.
              tg_saida-statusdesc = 'Sem Movimento'.

              IF p_monat-low <> '12'.

              ELSE.

                IF vg_found IS INITIAL.                  "2000004155 - IR172865
                  tg_saida-status = icon_light_out.
                  tg_saida-statusdesc = 'Não Iniciado'.
                ENDIF.                                   "2000004155 - IR172865

              ENDIF.

            ELSE.
              IF tg_saida-statusdesc = 'Sem Movimento'.
                tg_saida-status = icon_light_out.
                tg_saida-statusdesc = 'Não Iniciado'.
              ENDIF.
            ENDIF.
          ENDIF.

        ELSEIF tg_saida-statusdesc = 'Sem Movimento'.

          tg_saida-status = icon_light_out.
          tg_saida-statusdesc = 'Não Iniciado'.

        ENDIF.





        APPEND tg_saida.
        CLEAR: tg_saida, tg_zglt043, tg_zglt041 , tg_zglt039 , it_aprova, vg_nivel, it_t001 ,it_saldo_2 ,it_saldo_1, tg_zimp_cad_depto, it_saldo_3, vg_found.

      ENDLOOP.
    ENDIF.
    CLEAR: tg_saida , tg_zglt043, tg_zglt041 , tg_zglt039 , it_aprova, vg_nivel, it_t001 ,it_saldo_2 ,it_saldo_1, tg_zimp_cad_depto, it_saldo_3.
  ENDLOOP.

  SORT tg_saida BY saknr monat.
ENDFORM.                    " PROCESSA_DADOS
*&---------------------------------------------------------------------*
*&      Form  MONTA_ALV
*&---------------------------------------------------------------------*
FORM monta_alv .
  DATA: wl_layout     TYPE slis_layout_alv,
        wa_hints      TYPE alv_s_qinf,
        wl_disvariant TYPE disvariant. "USER STORY 163049 - MMSILVA - 08.01.2025

  IF e_mail IS NOT INITIAL.
    PERFORM enviar_email.
  ELSE.

    PERFORM: definir_eventos,
             montar_layout,
             construir_cabecalho USING 'H' TEXT-h02.

    CLEAR: it_hints[].

    wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
    wa_hints-value     = icon_resubmission.
    wa_hints-text      = TEXT-003. "'Não Vencido'.
    wa_hints-fieldname = 'STATUS_PRAZO'.
    APPEND wa_hints TO it_hints.

    wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
    wa_hints-value     = icon_alarm.
    wa_hints-text      = TEXT-004. "'Vencido'.
    wa_hints-fieldname = 'STATUS_PRAZO'.
    APPEND wa_hints TO it_hints.

    wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
    wa_hints-value     = icon_alert.
    wa_hints-text      = TEXT-005. "'Em Atraso'.
    wa_hints-fieldname = 'STATUS_PRAZO'.
    APPEND wa_hints TO it_hints.

    wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
    wa_hints-value     = icon_light_out.
    wa_hints-text      = TEXT-006. "'Não Iniciado'.
    wa_hints-fieldname = 'STATUS'.
    APPEND wa_hints TO it_hints.

    wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
    wa_hints-value     = icon_yellow_light.
    wa_hints-text      = TEXT-007. "'Aguardando liberação'.
    wa_hints-fieldname = 'STATUS'.
    APPEND wa_hints TO it_hints.

    wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
    wa_hints-value     = icon_green_light.
    wa_hints-text      = TEXT-008. "'Liberado'.
    wa_hints-fieldname = 'STATUS'.
    APPEND wa_hints TO it_hints.

    wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
    wa_hints-value     = icon_release.
    wa_hints-text      = TEXT-009. "'Aprovado'.
    wa_hints-fieldname = 'STATUS'.
    APPEND wa_hints TO it_hints.

    wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
    wa_hints-value     = icon_defect.
    wa_hints-text      = TEXT-010. "'Rejeitado'.
    wa_hints-fieldname = 'STATUS'.
    APPEND wa_hints TO it_hints.

    "Status Aprovação
    wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
    wa_hints-value     = icon_led_green.
    wa_hints-text      = TEXT-011. "'Liberado p/ aprovação próximo nível'.
    wa_hints-fieldname = 'STATUS_APROVA'.
    APPEND wa_hints TO it_hints.

    wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
    wa_hints-value     = icon_yellow_light.
    wa_hints-text      = TEXT-012. "'Recusado p/ verificação nível anterior'.
    wa_hints-fieldname = 'ICON_LED_RED'.
    APPEND wa_hints TO it_hints.

    wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
    wa_hints-value     = icon_led_yellow.
    wa_hints-text      = TEXT-013. "'Liberado p/ aprovação'.
    wa_hints-fieldname = 'STATUS_APROVA'.
    APPEND wa_hints TO it_hints.

    wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
    wa_hints-value     = icon_led_inactive.
    wa_hints-text      = TEXT-014. "'Não possui liberações/recusas'.
    wa_hints-fieldname = 'STATUS_APROVA'.
    APPEND wa_hints TO it_hints.

    wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
    wa_hints-value     = icon_release.
    wa_hints-text      = TEXT-015. "'Aprovada'.
    wa_hints-fieldname = 'STATUS_APROVA'.
    APPEND wa_hints TO it_hints.

    wl_layout-zebra = abap_true.

    wl_disvariant-variant = p_layout.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program      = sy-repid
        i_callback_user_command = 'USER_COMMAND'
        it_fieldcat             = estrutura[]
        is_layout               = wl_layout
        is_variant              = wl_disvariant
        i_save                  = 'A'                   "Modificação 23.01.2017
        it_events               = events
        it_except_qinfo         = it_hints
      TABLES
        t_outtab                = tg_saida.

  ENDIF.

ENDFORM.                    " MONTA_ALV
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
FORM definir_eventos .
  PERFORM carregar_eventos USING: slis_ev_top_of_page  'XTOP_OF_PAGE'.
ENDFORM.                    " DEFINIR_EVENTOS`
*&---------------------------------------------------------------------*
*&      Form  carregar_eventos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM carregar_eventos USING    name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.
ENDFORM.                    " CARREGAR_EVENTOS

*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
FORM montar_layout .

  DATA: vg_moeda_1        TYPE dd03p-scrtext_l,
        vg_moeda_2        TYPE dd03p-scrtext_l,
        vg_moeda_3        TYPE dd03p-scrtext_l,
        wa_moedas_empresa TYPE x001.

  READ TABLE it_t001 INDEX 1.

  CONCATENATE TEXT-016 it_t001-waers INTO vg_moeda_1 SEPARATED BY space. "'Total'

  CALL FUNCTION 'FI_CURRENCY_INFORMATION'
    EXPORTING
      i_bukrs = it_t001-bukrs
    IMPORTING
      e_x001  = wa_moedas_empresa.

  CONCATENATE TEXT-016 wa_moedas_empresa-hwae2 INTO vg_moeda_2 SEPARATED BY space. "'Total'

  CONCATENATE TEXT-016 wa_moedas_empresa-hwae3 INTO vg_moeda_3 SEPARATED BY space. "'Total'

  "// Fiedcat dos logs;

  IF ( p_inat = abap_true ).
    PERFORM montar_estrutura USING:
            01  ''                  'SEARCH'         'TG_SAIDA' 'SEARCH'                  ''                      '3' 'X',
            02  ''                  'STATUS'         'TG_SAIDA' 'STATUS'                  TEXT-017                '5' ' ', "'Status'
            03  ''                  'STATUSDESC'     'TG_SAIDA' 'STATUSDESC'              'Status Descrição'      '5' ' ', "'Status Descrição'
            04  'T001'              'BUKRS'          'TG_SAIDA' 'BUKRS'                   TEXT-001                '6' ' ', "'Empresa'
            05  'SKA1'              'SAKNR'          'TG_SAIDA' 'SAKNR'                   TEXT-018                ' ' ' ', "'Conta'
            06  ''                  'MONAT'          'TG_SAIDA' 'MONAT'                   TEXT-019                '4' ' ', "'Mês'
            07  'SKAT'              'TXT50'          'TG_SAIDA' 'TXT50'                   TEXT-020                ' ' ' ', "'Descrição'
            08  'FAGLFLEXT'         'HSL16'          'TG_SAIDA' 'SALDO_MI'                vg_moeda_1              '13' ' ',
            09  'FAGLFLEXT'         'KSL16'          'TG_SAIDA' 'SALDO_MI2'               vg_moeda_2              '13' ' ',
            10  ''                  'TX_FECH1'       'TG_SAIDA' 'TX_FECH1'                TEXT-021                '13' ' ', "'Taxa Fechamento'
            11  ''                  'DEP_RESP'       'TG_SAIDA' 'DEP_RESP'                TEXT-022                '30' ' ', "'Departamento'
            12  'ZGLT041'           'BNAME2'         'TG_SAIDA' 'BNAME'                   TEXT-023                '15' ' ', "'Responsável'
            13  ''                  'DT_ATUALIZACAO' 'TG_SAIDA' 'DT_ATUALIZACAO'          TEXT-024                '12' ' ', "'Dt. Modificação'
            14  ''                  'C_BALANCO'      'TG_SAIDA' 'C_BALANCO'               TEXT-025                '30' ' ', "'Classificação Balanço'
            15  ''                  'C_NOTA'         'TG_SAIDA' 'C_NOTA'                  TEXT-058                '30' ' '. "'Classificação Nota'
    "//  --

  ELSE.
    PERFORM montar_estrutura USING:
            01  ''                  'STATUS'         'TG_SAIDA' 'STATUS'                  TEXT-017                 '4'  ' ', "'Status'
            02  ''                  'STATUSDESC'     'TG_SAIDA' 'STATUSDESC'              'Status Descrição'       '5' ' ', "'Status Descrição'
            03  ''                  'STATUS_PRAZO'   'TG_SAIDA' 'STATUS_PRAZO'            TEXT-027                 '4'  ' ', "'Prazo'
            04  ''                  'STATUS_APROVA'  'TG_SAIDA' 'STATUS_APROVA'           TEXT-028                 '4'  'X', "'Status Aprovação'
            05  ''                  'NIVEL_APROVA'   'TG_SAIDA' 'NIVEL_APROVA'            TEXT-029                 '4'  ' ', "'Nível Aprovação'
            06  ''                  'DIAS_VENCIDO'   'TG_SAIDA' 'DIAS_VENCIDO'            TEXT-030                 '6'  ' ', "'Dias Vencimento'
            07  'T001'              'BUKRS'          'TG_SAIDA' 'BUKRS'                   TEXT-001                 '6'  ' ', "'Empresa'

            08  '    '              'MONAT'          'TG_SAIDA' 'MONAT'                   TEXT-019                 '4'  ' ', "'Mes'
            09  'SKA1'              'SAKNR'          'TG_SAIDA' 'SAKNR'                   TEXT-018                 ' '  ' ', "'Conta'
            10  'SKAT'              'TXT50'          'TG_SAIDA' 'TXT50'                   TEXT-020                 ' '  ' ', "'Descrição'
            11  'FAGLFLEXT'         'HSL16'          'TG_SAIDA' 'SALDO_MI'                vg_moeda_1               '13' ' ',
            12  'FAGLFLEXT'         'KSL16'          'TG_SAIDA' 'SALDO_MI2'               vg_moeda_2               '13' ' ',
            13  ''                  'TX_FECH1'       'TG_SAIDA' 'TX_FECH1'                TEXT-021                 '13' ' '. "'Taxa Fechamento'

    LOOP AT it_t001.
      IF it_t001-land1 NE 'BR'.
        PERFORM montar_estrutura USING:
                 14  'FAGLFLEXT'         'OSL16'          'TG_SAIDA' 'SALDO_MI3'               vg_moeda_3              '13' ' '.
        EXIT.
      ENDIF.
    ENDLOOP.

    PERFORM montar_estrutura USING:
             15  ''                  'DEP_RESP'         'TG_SAIDA' 'DEP_RESP'                TEXT-022           '30'  ' ', "'Departamento'
             16  'ZGLT041'           'BNAME2'           'TG_SAIDA' 'BNAME'                   TEXT-023           '15'  ' ', "'Responsável'
             17  ''                  'PRAZO_ENTR'       'TG_SAIDA' 'PRAZO_ENTR'              TEXT-031           '12'  ' ', "'Prazo Entrega'
             18  ''                  'DT_ATUALIZACAO'   'TG_SAIDA' 'DT_ATUALIZACAO'          TEXT-032           '12'  ' ', "'Dt. Atualização'
             19  ''                  'C_BALANCO'        'TG_SAIDA' 'C_BALANCO'               TEXT-056           '30'  ' ', " text-025 'Classificação Balanço'
             20  ''                  'C_NOTA'           'TG_SAIDA' 'C_NOTA'                  TEXT-058           '30'  ' ', "TEXT-026 'Classificação Nota'
             21  'ZGLT041'           'CTA_MONET'        'TG_SAIDA' 'CTA_MONET'               TEXT-033           ' '   ' ', "'Monetária'
             22  'ZGLT041'           'CTA_INTERCOMPANY' 'TG_SAIDA' 'CTA_INTERCOMPANY'        TEXT-034           ' '   ' ', "'Intercompany'
             23  ''                  'USUARIO_RESPO'    'TG_SAIDA' 'USUARIO_RESPO'           TEXT-057           '100' ' '. "TEXT-035 'Usuário(s) Resp. Reconciliação'
  ENDIF.

ENDFORM.                    " MONTAR_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_hotspot).

  CLEAR wa_estrutura.

** Utiliza para retirar os ZEROS.
*  IF p_field EQ 'BELNR'
*  OR p_field EQ 'BELNR_EST'.
*    wa_estrutura-just = 'C'.
*    wa_estrutura-hotspot = 'X'.
*  ENDIF.

  wa_estrutura-outputlen     = p_outputlen.
  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-hotspot       = p_hotspot.

  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.

  TRANSLATE  wa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_fieldname TO UPPER CASE.

  IF p_ref_fieldname = 'DIAS_VENCIDO'.
    wa_estrutura-just = 'R'.
  ENDIF.

  IF p_ref_fieldname = 'STATUS' OR p_ref_fieldname = 'STATUS_PRAZO' OR p_ref_fieldname = 'STATUS_APROVA' OR p_ref_fieldname = 'SEARCH'.
    wa_estrutura-icon = 'X'.
    wa_estrutura-just = 'C'.
  ENDIF.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " MONTAR_ESTRUTURA
*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xtop_of_page.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top
      i_logo             = ''.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  construir_cabecalho
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TYP        text
*      -->TEXT       text
*----------------------------------------------------------------------*
FORM construir_cabecalho USING typ text.

  DATA: ls_line TYPE slis_listheader,
*---> 20.06.2023 - Migração S4 - DG
        "        vg_line TYPE char02,
        vg_line TYPE zchar02,
*<--- 20.06.2023 - Migração S4 - DG
        lin     TYPE i.

  CLEAR: vg_line.

  ls_line-typ = typ.
  ls_line-info = text.
  APPEND ls_line TO t_top.

  DATA: wl_butxt TYPE t001-butxt,
        wl_dep   TYPE c LENGTH 20,
        wl_resp  TYPE c LENGTH 20.

  CLEAR: wl_butxt, wl_dep, wl_resp, t_top.

  DESCRIBE TABLE it_t001 LINES lin.

  IF lin EQ 1.
    READ TABLE it_t001 INDEX 1.
    wl_butxt = it_t001-butxt.
    ls_line-typ = 'S'..
    ls_line-key = TEXT-036. "'Empresa:'.
    CONCATENATE  it_t001-bukrs '-' wl_butxt INTO ls_line-info SEPARATED BY space.
    APPEND ls_line TO t_top.
    CLEAR ls_line-info.
  ELSE.
    ls_line-typ = 'S'.
    ls_line-key = TEXT-036. "'Empresa:'.
    CLEAR: ls_line-info.
    LOOP AT it_t001.
      lin = strlen( ls_line-info ).
      IF lin GE 55.
        APPEND ls_line TO t_top.
        CLEAR ls_line-info.
      ENDIF.

      IF ls_line-info IS INITIAL.
        ls_line-info = it_t001-bukrs.
      ELSE.
        CONCATENATE ls_line-info ',' INTO ls_line-info.
        CONCATENATE ls_line-info it_t001-bukrs INTO ls_line-info SEPARATED BY space.
      ENDIF.
    ENDLOOP.
    APPEND ls_line TO t_top.
    CLEAR ls_line-info.
  ENDIF.

  IF NOT p_dep_re IS INITIAL.
    SELECT SINGLE dep_resp_desc
      FROM zimp_cad_depto INTO wl_dep
      WHERE dep_resp = p_dep_re.
  ENDIF.
  ls_line-typ = 'S'.
  ls_line-key = TEXT-037. "'Departamento:'.
  CONCATENATE  p_dep_re '-' wl_dep INTO ls_line-info SEPARATED BY space.
  APPEND ls_line TO t_top.
  CLEAR ls_line-info.

  IF NOT p_bname IS INITIAL.
    SELECT SINGLE useralias FROM usrefus
      INTO wl_resp
      WHERE bname = p_bname.
  ENDIF.

  ls_line-typ = 'S'.
  ls_line-key = TEXT-038. "'Responsável:'.
  CONCATENATE  p_bname '-' wl_resp INTO ls_line-info SEPARATED BY space.
  APPEND ls_line TO t_top.
  CLEAR ls_line-info.

  "Ano

*  LS_LINE-KEY = TEXT-039. "'Mês/Ano:'.
*  CONCATENATE  P_MONAT '/' P_GJAHR  INTO LS_LINE-INFO SEPARATED BY SPACE.
*  APPEND LS_LINE TO T_TOP.
*  CLEAR LS_LINE-INFO.

  ls_line-key = TEXT-060. "'Mês/Ano:'.
  ls_line-info = p_gjahr.
*  CONCATENATE  P_MONAT '/' P_GJAHR  INTO LS_LINE-INFO SEPARATED BY SPACE.
  APPEND ls_line TO t_top.
  CLEAR ls_line-info.

  "Mes.

  READ TABLE p_monat INTO DATA(ws_monat) INDEX 1.
  DATA(ms_inic) = ws_monat-low.

  SORT p_monat DESCENDING BY low.
  READ TABLE p_monat INTO DATA(w_monat) INDEX 1.
  DATA(ms_fim) = w_monat-low.

  ls_line-key = TEXT-059. "'Mês/Ano:'.
  CONCATENATE  ms_inic '-' ms_fim  INTO ls_line-info SEPARATED BY space.
  APPEND ls_line TO t_top.
  CLEAR ls_line-info.
ENDFORM.                    " CONSTRUIR_CABECALHO

*&---------------------------------------------------------------------*
*&      Form  ENVIAR_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM enviar_email .

  DATA: bsmtp_addr        TYPE adr6-smtp_addr,
        doc_chng          LIKE sodocchgi1,
        it_skat           TYPE TABLE OF skat WITH HEADER LINE,
        objtxt            LIKE solisti1   OCCURS 10 WITH HEADER LINE,
        objpack           LIKE sopcklsti1 OCCURS  2 WITH HEADER LINE,
        reclist           LIKE somlreci1  OCCURS  5 WITH HEADER LINE,
        wa_zimp_cad_depto TYPE zimp_cad_depto,
        e_x001            TYPE x001,
        tab_lines         LIKE sy-tabix,
        ck_enviar         TYPE char01,
        ctotal(20),
        vdata(10).

  SELECT * INTO TABLE it_zglt058
    FROM zglt058
   WHERE bukrs    IN p_bukrs
     AND dep_resp EQ p_dep_re.

  SELECT * INTO TABLE it_t001
    FROM t001
   WHERE bukrs IN p_bukrs.

  READ TABLE it_t001 INDEX 1.

  SELECT * INTO TABLE it_skat
    FROM skat
   WHERE spras EQ sy-langu.

  SORT it_skat BY ktopl saknr.

  SELECT SINGLE * INTO wa_zimp_cad_depto
    FROM zimp_cad_depto
   WHERE dep_resp EQ p_dep_re.

  doc_chng-obj_name = 'LOG_RECONC'.
  doc_chng-obj_descr = TEXT-040. "'Reconciliação - Em atraso'.

  CALL FUNCTION 'FI_CURRENCY_INFORMATION'
    EXPORTING
      i_bukrs                = it_t001-bukrs
    IMPORTING
      e_x001                 = e_x001
    EXCEPTIONS
      currency_2_not_defined = 1
      currency_3_not_defined = 2
      OTHERS                 = 3.

  LOOP AT it_zglt058.

    ck_enviar = abap_false.

    SELECT SINGLE adr6~smtp_addr INTO bsmtp_addr
      FROM usr21
     INNER JOIN adr6 ON  usr21~addrnumber = adr6~addrnumber AND usr21~persnumber = adr6~persnumber
     WHERE usr21~bname = it_zglt058-bname.

    IF ( sy-subrc IS NOT INITIAL ) OR ( bsmtp_addr IS INITIAL ).
      CONTINUE.
    ENDIF.

    CLEAR: objtxt[].
    CONCATENATE TEXT-036 it_t001-bukrs '-' it_t001-butxt INTO objtxt-line SEPARATED BY space. "'Empresa:'
    APPEND objtxt.
    CONCATENATE TEXT-037 wa_zimp_cad_depto-dep_resp '-' wa_zimp_cad_depto-dep_resp_desc INTO objtxt-line SEPARATED BY space. "'Departamento:'
    APPEND objtxt.
    CONCATENATE p_monat '-' p_gjahr INTO objtxt-line.
    CONCATENATE TEXT-041 objtxt-line INTO objtxt-line SEPARATED BY space. "'Mês-Ano:'
    APPEND objtxt.

    objtxt-line = ''. APPEND objtxt.
    objtxt-line = TEXT-042. "'Estão em atraso as reconciliações das contas:'
    APPEND objtxt.
    objtxt-line = ''. APPEND objtxt.
    objtxt-line = TEXT-043. "'-------------------------------------------------------------------------------------------------------' .
    APPEND objtxt.

    LOOP AT tg_saida WHERE dep_resp(2) EQ it_zglt058-dep_resp.

      ck_enviar = abap_true.

      READ TABLE it_skat WITH KEY ktopl = it_t001-ktopl
                                  saknr = tg_saida-saknr.

      CONCATENATE TEXT-044 tg_saida-saknr '-' it_skat-txt50 INTO objtxt-line SEPARATED BY space. "'Conta:'
      APPEND objtxt.

      "1ª Moeda da Empresa
      WRITE tg_saida-saldo_mi TO ctotal CURRENCY it_t001-waers.
      CONDENSE ctotal NO-GAPS.
      CONCATENATE TEXT-045 it_t001-waers ctotal INTO objtxt-line SEPARATED BY space. "'1ª Moeda: '
      APPEND objtxt.

      "2ª Moeda da Empresa
      WRITE tg_saida-saldo_mi2 TO ctotal CURRENCY e_x001-hwae2.
      CONDENSE ctotal NO-GAPS.
      CONCATENATE TEXT-046 e_x001-hwae2 ctotal INTO objtxt-line SEPARATED BY space. "'2ª Moeda: '
      APPEND objtxt.

      "Somente 3º moeda em paises diferente de Brasil
      IF it_t001-land1 NE 'BR'.
        "3ª Moeda da Empresa
        WRITE tg_saida-saldo_mi3 TO ctotal CURRENCY e_x001-hwae3.
        CONDENSE ctotal NO-GAPS.
        CONCATENATE TEXT-047 e_x001-hwae3 ctotal INTO objtxt-line SEPARATED BY space. "'3ª Moeda: '
        APPEND objtxt.
      ENDIF.
      objtxt-line = TEXT-043. "'-------------------------------------------------------------------------------------------------------' .
      APPEND objtxt.

    ENDLOOP.

    IF ck_enviar EQ abap_true.
*     Setar tamanho da mensagem
      DESCRIBE TABLE objtxt LINES tab_lines.
      READ TABLE objtxt INDEX tab_lines.
      doc_chng-doc_size = ( tab_lines - 1 ) * 255 + strlen( objtxt ).

*     Criar entrada de documento comprimido
      CLEAR: objpack[], reclist[].
      CLEAR objpack-transf_bin.
      objpack-head_start = 1.
      objpack-head_num   = 0.
      objpack-body_start = 1.
      objpack-body_num   = tab_lines.
      objpack-doc_type   = 'RAW'.
      APPEND objpack.

      reclist-receiver = bsmtp_addr.
      reclist-rec_type = 'U'.                    "Define email externo
      APPEND reclist.

      CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
        EXPORTING
          document_data              = doc_chng
          put_in_outbox              = 'X'
          commit_work                = 'X'
        TABLES
          packing_list               = objpack
          contents_txt               = objtxt      "CONTENTS_HEX = CONTENT_HEX
          receivers                  = reclist
        EXCEPTIONS
          too_many_receivers         = 1
          document_not_sent          = 2
          document_type_not_exist    = 3
          operation_no_authorization = 4
          parameter_error            = 5
          x_error                    = 6
          enqueue_error              = 7
          OTHERS                     = 8.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " ENVIAR_EMAIL

*&---------------------------------------------------------------------*
*&      Form  BUSCA_USUARIOS_RECONCILIACAO
*&---------------------------------------------------------------------*
*       Retorna usuário responsável pela reconsiliação
*----------------------------------------------------------------------*
FORM busca_usuarios_reconciliacao  USING    p_bukrs    TYPE bukrs
                                            p_dep_resp TYPE zdep_resp
                                            p_saknr    TYPE saknr
                                   CHANGING r_usuarios.

  CLEAR: r_usuarios.

  LOOP AT it_zglt063 WHERE bukrs    EQ p_bukrs
                       AND dep_resp EQ p_dep_resp
                       AND saknr    EQ p_saknr.
    IF r_usuarios IS INITIAL.
      r_usuarios = it_zglt063-bname.
    ELSE.
      CONCATENATE r_usuarios ',' INTO r_usuarios.
      CONCATENATE r_usuarios it_zglt063-bname INTO r_usuarios SEPARATED BY space.
    ENDIF.
  ENDLOOP.

  LOOP AT it_zglt062 WHERE bukrs    EQ p_bukrs
                       AND dep_resp EQ p_dep_resp.

    READ TABLE it_zglt063 WITH KEY bukrs    = p_bukrs
                                   dep_resp = p_dep_resp
                                   saknr    = p_saknr
                                   BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.
      IF r_usuarios IS INITIAL.
        r_usuarios = it_zglt062-bname.
      ELSE.
        CONCATENATE r_usuarios ',' INTO r_usuarios.
        CONCATENATE r_usuarios it_zglt062-bname INTO r_usuarios SEPARATED BY space.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " BUSCA_USUARIOS_RECONCILIACAO

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       Quando clica no link
*----------------------------------------------------------------------*
FORM user_command  USING ucomm LIKE sy-ucomm
      selfield TYPE slis_selfield.

  READ TABLE tg_saida INDEX selfield-tabindex.

  CASE selfield-fieldname.
    WHEN 'STATUS_APROVA'.
      CALL SCREEN 0001 STARTING AT 30 08.
    WHEN 'SEARCH'.
      PERFORM view_doc USING tg_saida-seq
                             tg_saida-bukrs
                             tg_saida-dep_resp
                             tg_saida-monat
                             tg_saida-gjahr
                             tg_saida-saknr.
  ENDCASE.
ENDFORM.                    "user_command

*&---------------------------------------------------------------------*
*&      Form  VIEW_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->WL_043     text
*----------------------------------------------------------------------*
FORM view_doc USING p_seq
                    p_bukrs
                    p_dep_resp
                    p_monat
                    p_gjahr
                    p_saknr.

  RANGES: s_bukrs FOR zglt042-empresa_de,
          s_depre FOR zglt042-dep_resp,
          s_mes   FOR zglt042-mes_de,
          s_ano   FOR zglt042-ano_de,
          s_contas FOR zglt041-saknr,
          s_seq   FOR zglt043-seq.

  s_seq-sign   = 'I'.
  s_seq-option = 'EQ'.
  s_seq-low    = p_seq.
  s_seq-high   = p_seq.
  APPEND s_seq.

  s_bukrs-sign   = 'I'.
  s_bukrs-option = 'EQ'.
  s_bukrs-low    = p_bukrs.
  s_bukrs-high   = p_bukrs.
  APPEND s_bukrs.

  s_depre-sign   = 'I'.
  s_depre-option = 'EQ'.
  s_depre-low    = p_dep_resp.
  s_depre-high   = p_dep_resp.
  APPEND s_depre.

  s_mes-sign   = 'I'.
  s_mes-option = 'EQ'.
  s_mes-low    = p_monat.
  s_mes-high   = p_monat.
  APPEND s_mes.

  s_ano-sign   = 'I'.
  s_ano-option = 'EQ'.
  s_ano-low    = p_gjahr.
  s_ano-high   = p_gjahr.
  APPEND s_ano.

  s_contas-sign   = 'I'.
  s_contas-option = 'EQ'.
  s_contas-low    = p_saknr.
  s_contas-high   = p_saknr.
  APPEND s_contas.

  SUBMIT zgl021 USING SELECTION-SCREEN '1000'
                 WITH s_bukrs  IN s_bukrs
                 WITH s_depre  IN s_depre
                 WITH s_mes    IN s_mes
                 WITH s_ano    IN s_ano
                 WITH s_contas IN s_contas
                 WITH s_seq    IN s_seq
                 AND RETURN.
ENDFORM.                    " VIEW_DOC

INCLUDE zgl019_0001.

*USER STORY 163049 - MMSILVA - 08.01.2025 - Inicio
FORM f_alv_variant_f4 CHANGING pa_vari.

  DATA: rs_variant LIKE disvariant.

  rs_variant-report   = sy-repid.
  rs_variant-username = sy-uname.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = rs_variant
      i_save     = 'A'
    IMPORTING
      es_variant = rs_variant
    EXCEPTIONS
      OTHERS     = 1.
  IF sy-subrc = 0.
    pa_vari = rs_variant-variant.
  ENDIF.

ENDFORM.
*USER STORY 163049 - MMSILVA - 08.01.2025 - Fim
