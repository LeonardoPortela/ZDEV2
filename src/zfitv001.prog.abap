*&---------------------------------------------------------------------*
*& Report  ZFITV001
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfitv001.

*---------------------------------------------------------------------------
* Estrutura
*---------------------------------------------------------------------------

TYPES: BEGIN OF ty_ftpt_req_head,
        pernr          TYPE ftpt_req_head-pernr,
        reinr          TYPE ftpt_req_head-reinr,
        requestvrs     TYPE ftpt_req_head-requestvrs,
        plan_request   TYPE ftpt_req_head-plan_request,
        date_beg       TYPE ftpt_req_head-date_beg,
        date_end       TYPE ftpt_req_head-date_end,
        location_end   TYPE ftpt_req_head-location_end,
        request_reason TYPE ftpt_req_head-request_reason,
        exbel          TYPE ptrv_doc_it-exbel,
        ename          TYPE pa0001-ename,
       END OF ty_ftpt_req_head,

       BEGIN OF ty_ptrv_doc_it,
        awref TYPE ptrv_doc_it-awref,
        aworg TYPE ptrv_doc_it-aworg,
        awlin TYPE ptrv_doc_it-awlin,
        pernr TYPE ptrv_doc_it-pernr,
        bukrs TYPE ptrv_doc_it-bukrs,
        gsber TYPE ptrv_doc_it-gsber,
        kostl TYPE ptrv_doc_it-kostl,
        aufnr TYPE ptrv_doc_it-aufnr,
        wrbtr TYPE ptrv_doc_it-wrbtr,
        lifnr TYPE ptrv_doc_it-lifnr,
       END OF ty_ptrv_doc_it,

       BEGIN OF ty_t706b5,
        spras TYPE t706b5-spras,
        morei TYPE t706b5-morei,
        spkzl TYPE t706b5-spkzl,
        sptxt TYPE t706b5-sptxt,
       END OF ty_t706b5,

       BEGIN OF ty_t706b4,
        morei TYPE t706b4-morei,
        spkzl TYPE t706b4-spkzl,
        payot TYPE t706b4-payot,
        endda TYPE t706b4-endda,
        lgarl TYPE t706b4-lgarl,
       END OF ty_t706b4,

       BEGIN OF ty_t706k,
        morei TYPE t706k-morei,
        lgart TYPE t706k-lgart,
        users TYPE t706k-users,
        endda TYPE t706k-endda,
        kont1 TYPE t706k-kont1,
        komok TYPE t030-komok,
       END OF ty_t706k,

       BEGIN OF ty_t030,
        ktopl TYPE t030-ktopl,
        ktosl TYPE t030-ktosl,
        bwmod TYPE t030-bwmod,
        komok TYPE t030-komok,
        bklas TYPE t030-bklas,
*       KONT1 TYPE T030-KONT1,
        konts TYPE t030-konts,
        txt20 TYPE skat-txt20,
       END OF ty_t030.


* receipt data
DATA: BEGIN OF beleg OCCURS 20.        "Tabelle Einzelbelege
        INCLUDE STRUCTURE ptk03.
DATA: END OF beleg.

DATA: BEGIN OF exbel OCCURS 20.        "Tabelle Einzelbelege
        INCLUDE STRUCTURE ptk33.
DATA: END OF exbel.

DATA: BEGIN OF konti OCCURS 10.        "Tabelle Kontierung
        INCLUDE STRUCTURE ptk17.
DATA: END OF konti.

DATA: header TYPE ptk03.
DATA: BEGIN OF te-key.                 "Schlüssel TE-Cluster
        INCLUDE STRUCTURE ptp00.
DATA: END OF te-key.

DATA: BEGIN OF ote-version.            "TE-Version auf PCL1
        INCLUDE STRUCTURE ptk01.
DATA: END OF ote-version.

DATA: BEGIN OF t_item OCCURS 0.
        INCLUDE STRUCTURE zfitv001.
DATA: END OF t_item.

DATA: BEGIN OF te-version,             "TE-Version Datenbeschreibung
        saprl LIKE ote-version-saprl,
*       numbr like ote-version-numbr value '01',     "version till 2.2Z
*       numbr like ote-version-numbr value '02',     "version till 3.1Z
        numbr LIKE ote-version-numbr VALUE '03',     "version for 4.0
      END OF te-version.



DATA: rp-exp-te-subrc LIKE sy-subrc,
      rp-imp-te-subrc LIKE sy-subrc.

* Import TE-Cluster from PCL1
DEFINE rp-imp-c1-te.
  set extended check off.                                   "MAWK070411
  import te-version to ote-version
         beleg
         exbel
         konti
  from   database pcl1(te)
  id     te-key.
  rp-imp-te-subrc = sy-subrc.
  set extended check on.                                    "MAWK070411
END-OF-DEFINITION.



TYPES: BEGIN OF ty_header,
        bukrs          TYPE ptrv_doc_it-bukrs,
        butxt          TYPE t001-butxt,
        gsber          TYPE ptrv_doc_it-gsber,
        gtext          TYPE tgsbt-gtext,
        pernr          TYPE pa0001-pernr,
        ename          TYPE pa0001-ename,
        bankl          TYPE lfbk-bankl,
        bankn          TYPE lfbk-bankn,
        date_beg       TYPE ftpt_req_head-date_beg,
        date_end       TYPE ftpt_req_head-date_end,
        location_end   TYPE ftpt_req_head-location_end,
        request_reason TYPE ftpt_req_head-request_reason,
        kostl          TYPE ptk_cost_dist-kostl,
        ktext1         TYPE cskt-ktext,
        kostl12        TYPE ptk_cost_dist-kostl,
        ktext12        TYPE cskt-ktext,
        kostl13        TYPE ptk_cost_dist-kostl,
        ktext13        TYPE cskt-ktext,
        kostl14        TYPE ptk_cost_dist-kostl,
        ktext14        TYPE cskt-ktext,
        aufnr          TYPE ptk_cost_dist-aufnr,
        ktext2         TYPE coas-ktext,
        aufnr22        TYPE ptk_cost_dist-aufnr,
        ktext22        TYPE coas-ktext,
        aufnr23        TYPE ptk_cost_dist-aufnr,
        ktext23        TYPE coas-ktext,
        aufnr24        TYPE ptk_cost_dist-aufnr,
        ktext24        TYPE coas-ktext,
        wrbtr          TYPE ptrv_doc_it-wrbtr,
        reinr TYPE ftpt_req_head-reinr,
       END OF ty_header,

       BEGIN OF ty_ptrv_head,
         pernr TYPE ptrv_head-pernr,
         reinr TYPE ptrv_head-reinr,
         datv1 TYPE ptrv_head-datv1,
         datb1 TYPE ptrv_head-datb1,
         zort1 TYPE ptrv_head-zort1,
         kunde TYPE ptrv_head-kunde,
         ename TYPE pa0001-ename,
       END OF ty_ptrv_head,

       BEGIN OF ty_pa0001.
        INCLUDE STRUCTURE pa0001.
TYPES: END OF ty_pa0001.


*---------------------------------------------------------------------------
* Tabela interna
*---------------------------------------------------------------------------

DATA: BEGIN OF t_skat OCCURS 0.
        INCLUDE STRUCTURE skat.
DATA: END OF t_skat.

DATA: t_ftpt_req_head TYPE TABLE OF ty_ftpt_req_head,
      t_ptrv_doc_it   TYPE TABLE OF ty_ptrv_doc_it,
      t_t706b5        TYPE TABLE OF ty_t706b5,
      t_t706b4        TYPE TABLE OF ty_t706b4,
      t_t706k         TYPE TABLE OF ty_t706k,
      t_t030          TYPE TABLE OF ty_t030,
      t_header        TYPE TABLE OF ty_header,
      t_ptrv_head     TYPE TABLE OF ty_ptrv_head,
      t_pa0001        TYPE TABLE OF ty_pa0001.

DATA: BEGIN OF t_perio OCCURS 100.  "Table of periods
        INCLUDE STRUCTURE ptp42.                            "XOWK000899
DATA: END OF t_perio.
*---------------------------------------------------------------------------
* Work area
*---------------------------------------------------------------------------

DATA: wa_ftpt_req_head TYPE ty_ftpt_req_head,
      wa_ptrv_doc_it   TYPE ty_ptrv_doc_it,
      wa_t706b5        TYPE ty_t706b5,
      wa_t706b4        TYPE ty_t706b4,
      wa_t706k         TYPE ty_t706k,
      wa_t030          TYPE ty_t030,
      wa_header        TYPE ty_header,
      wa_ptrv_head     TYPE ty_ptrv_head,
      wa_pa0001        TYPE ty_pa0001,

vg_fm_name         TYPE rs38l_fnam. "Nome da função smart form

DATA: w_saldo  TYPE ty_header-wrbtr,
      w_saldoe TYPE ty_header-wrbtr,
      w_desp   TYPE ty_header-wrbtr.
*---------------------------------------------------------------------------
* Tela de seleção
*---------------------------------------------------------------------------

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_pernr TYPE ftpt_req_head-pernr OBLIGATORY MEMORY ID per,
            p_reinr TYPE ftpt_req_head-reinr OBLIGATORY MATCHCODE OBJECT sh_trip_numbers.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN ON p_reinr.

  SET PARAMETER ID 'PER' FIELD p_pernr.

START-OF-SELECTION.

  PERFORM f_seleciona_dados.

  PERFORM f_processa_dados.

  PERFORM f_sapscript.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM f_seleciona_dados .

  SELECT pernr reinr requestvrs plan_request date_beg date_end
         location_end request_reason
    FROM ftpt_req_head
    INTO TABLE t_ftpt_req_head
    WHERE pernr EQ p_pernr
    AND   reinr EQ p_reinr.

  LOOP AT t_ftpt_req_head INTO wa_ftpt_req_head.
    MOVE wa_ftpt_req_head-reinr TO wa_ftpt_req_head-exbel.
    SELECT SINGLE ename FROM pa0001 INTO wa_ftpt_req_head-ename
      WHERE pernr = wa_ftpt_req_head-pernr.
    MODIFY t_ftpt_req_head FROM wa_ftpt_req_head.
  ENDLOOP.

  IF sy-subrc IS INITIAL.
    SELECT  awref aworg awlin pernr bukrs gsber kostl aufnr wrbtr lifnr
      FROM ptrv_doc_it
      INTO TABLE t_ptrv_doc_it
      FOR ALL ENTRIES IN t_ftpt_req_head
      WHERE pernr EQ t_ftpt_req_head-pernr
        AND exbel EQ t_ftpt_req_head-exbel
        AND antrg EQ '2'
        AND hkont EQ '0000113124'.
  ENDIF.



*   FORM read_te_cluster USING te_pernr te_reinr te_perio te_pdvrs.
*   PERFORM clear_all.                   "Alle Tabellen loeschen
* TE-Schluessel fuellen
  te-key-pernr = p_pernr.
  te-key-reinr = p_reinr.
*   te-key-perio = te_perio.

  CALL FUNCTION 'TRIPS_READ_PERIO'
    EXPORTING
      employeenumber         = p_pernr
      old_versions           = ''
* IMPORTING
*   NO_TRIPS               =
    TABLES
      perio                  = t_perio.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.

    DELETE t_perio WHERE reinr NE p_reinr.
    READ TABLE t_perio INDEX 1.

    IF t_ptrv_doc_it[] IS INITIAL.
*      SELECT  awref aworg awlin pernr bukrs gsber kostl aufnr wrbtr lifnr
*        FROM ptrv_doc_it
*        INTO TABLE t_ptrv_doc_it
*        FOR ALL ENTRIES IN t_perio
*        WHERE pernr EQ t_perio-pernr
**        AND exbel EQ t_perio-exbel
*          AND antrg EQ '2'
*          AND hkont EQ '0000113124'.

      SELECT *
        INTO TABLE t_pa0001
        FROM pa0001
        FOR ALL ENTRIES IN t_perio
        WHERE pernr EQ t_perio-pernr.

      IF sy-subrc EQ 0.

        SELECT pernr reinr datv1 datb1 zort1 kunde
          INTO TABLE t_ptrv_head
          FROM ptrv_head
          FOR ALL ENTRIES IN t_perio
          WHERE pernr EQ t_perio-pernr
            AND reinr EQ t_perio-reinr.

        IF sy-subrc EQ 0.

          SORT t_ptrv_head BY pernr reinr.

          LOOP AT t_ptrv_head INTO wa_ptrv_head.

            READ TABLE t_pa0001 INTO wa_pa0001 WITH KEY pernr = wa_ptrv_head-pernr.

            IF sy-subrc EQ 0.

              MOVE wa_pa0001-ename TO wa_ptrv_head-ename.

            ENDIF.

            MODIFY t_ptrv_head FROM wa_ptrv_head.

          ENDLOOP.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.

  te-key-pdvrs = t_perio-pdvrs.
  rp-imp-c1-te.

*  IF sy-subrc IS INITIAL.
  IF NOT beleg[] IS INITIAL.


*
    SELECT spras morei spkzl sptxt
      FROM t706b5
      INTO TABLE t_t706b5
       FOR ALL ENTRIES IN beleg
      WHERE spras = sy-langu AND
            morei = '37'     AND
            spkzl = beleg-spkzl.
  ENDIF.
  IF sy-subrc IS INITIAL.
    SELECT morei spkzl payot endda lgarl
      FROM t706b4
      INTO TABLE t_t706b4
      FOR ALL ENTRIES IN beleg
      WHERE morei = '37'     AND
            spkzl = beleg-spkzl.
  ENDIF.
  IF sy-subrc IS INITIAL.
    SELECT morei lgart users endda kont1
      FROM t706k
      INTO TABLE t_t706k
      FOR ALL ENTRIES IN t_t706b4
      WHERE morei = '37' AND
            lgart = t_t706b4-lgarl.
  ENDIF.
  IF sy-subrc IS INITIAL.

    LOOP AT t_t706k INTO wa_t706k.
      CONCATENATE '1' wa_t706k-kont1+1 INTO wa_t706k-komok.
      MODIFY t_t706k FROM wa_t706k.
    ENDLOOP.
    SELECT t030~ktopl t030~ktosl t030~bwmod t030~komok
           t030~bklas t030~konts
*           skat~txt20
      FROM t030 "INNER JOIN skat on
*      ( t030~ktopl eq skat~KTOPL and
*        t030~konts eq skat~SAKNR )
      INTO TABLE t_t030
      FOR ALL ENTRIES IN t_t706k
      WHERE t030~ktopl = '0050' AND
            t030~ktosl = 'HRT'  AND
            t030~komok = t_t706k-komok." and
*            skat~spras = sy-langu.

    SELECT * FROM skat INTO TABLE t_skat
      FOR ALL ENTRIES IN t_t030
      WHERE spras = sy-langu AND
            ktopl = t_t030-ktopl AND
            saknr = t_t030-konts.
  ENDIF.
ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_PROCESSA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_processa_dados .

  DATA: w_lifnr TYPE lfb1-lifnr.

  CLEAR t_header. REFRESH t_header.

  LOOP AT t_ftpt_req_head INTO wa_ftpt_req_head.
    MOVE-CORRESPONDING wa_ftpt_req_head TO wa_header.
    READ TABLE t_ptrv_doc_it INTO wa_ptrv_doc_it INDEX 1.
    MOVE-CORRESPONDING wa_ptrv_doc_it TO wa_header.

    SELECT SINGLE butxt FROM t001 INTO wa_header-butxt
      WHERE bukrs = wa_header-bukrs.

    SELECT SINGLE gtext FROM tgsbt INTO wa_header-gtext
      WHERE spras = sy-langu AND
            gsber = wa_header-gsber.

    LOOP AT konti.
      CASE sy-tabix.

        WHEN 1.

          SELECT SINGLE ktext FROM coas INTO wa_header-ktext2
             WHERE aufnr = konti-aufnr.
          wa_header-aufnr = konti-aufnr.
          SELECT SINGLE ktext FROM cskt INTO wa_header-ktext1
            WHERE spras = sy-langu AND
                  kokrs = konti-kokrs AND
                  kostl = konti-kostl.
          wa_header-kostl = konti-kostl.
        WHEN 2.

          SELECT SINGLE ktext FROM coas INTO wa_header-ktext22
   WHERE aufnr = konti-aufnr.
          wa_header-aufnr22 = konti-aufnr.

          SELECT SINGLE ktext FROM cskt INTO wa_header-ktext12
            WHERE spras = sy-langu AND
                  kokrs = konti-kokrs AND
                  kostl = konti-kostl.
          wa_header-kostl12 = konti-kostl.
        WHEN 3.
          SELECT SINGLE ktext FROM coas INTO wa_header-ktext23
             WHERE aufnr = konti-aufnr.
          wa_header-aufnr23 = konti-aufnr.
          SELECT SINGLE ktext FROM cskt INTO wa_header-ktext13
            WHERE spras = sy-langu AND
                  kokrs = konti-kokrs AND
                  kostl = konti-kostl.
          wa_header-kostl13 = konti-kostl.
        WHEN 4.

          SELECT SINGLE ktext FROM coas INTO wa_header-ktext24
             WHERE aufnr = konti-aufnr.
          wa_header-aufnr24 = konti-aufnr.

          SELECT SINGLE ktext FROM cskt INTO wa_header-ktext14
            WHERE spras = sy-langu AND
                  kokrs = konti-kokrs AND
                  kostl = konti-kostl.
          wa_header-kostl14 = konti-kostl.
        WHEN OTHERS.
          EXIT.
      ENDCASE.

    ENDLOOP.

    SELECT SINGLE lifnr FROM lfb1 INTO w_lifnr
      WHERE bukrs = wa_header-bukrs AND
            pernr = wa_header-pernr.

    SELECT SINGLE bankl bankn FROM lfbk
      INTO (wa_header-bankl, wa_header-bankn)
      WHERE lifnr = w_lifnr.

    APPEND wa_header TO t_header.
  ENDLOOP.

  IF  t_header IS INITIAL.

    LOOP AT t_perio.
      READ TABLE t_pa0001 INTO wa_pa0001 WITH KEY pernr = t_perio-pernr.
      MOVE-CORRESPONDING wa_pa0001 TO wa_header.
      MOVE-CORRESPONDING t_perio TO wa_header.
      READ TABLE t_ptrv_doc_it INTO wa_ptrv_doc_it INDEX 1.
*      MOVE-CORRESPONDING wa_ptrv_doc_it TO wa_header.

      READ TABLE t_ptrv_head INTO wa_ptrv_head WITH KEY pernr = t_perio-pernr
                                                        reinr = t_perio-reinr
                                               BINARY SEARCH.

      IF sy-subrc EQ 0.

        MOVE wa_ptrv_head-datv1 TO wa_header-date_beg.
        MOVE wa_ptrv_head-datb1 TO wa_header-date_end.
        MOVE wa_ptrv_head-zort1 TO wa_header-location_end.
        MOVE wa_ptrv_head-kunde TO wa_header-request_reason.
        MOVE wa_ptrv_head-ename TO wa_header-ename.

      ENDIF.

      SELECT SINGLE butxt FROM t001 INTO wa_header-butxt
        WHERE bukrs = wa_header-bukrs.

      SELECT SINGLE gtext FROM tgsbt INTO wa_header-gtext
        WHERE spras = sy-langu AND
              gsber = wa_header-gsber.

      LOOP AT konti.
        CASE sy-tabix.

          WHEN 1.

            SELECT SINGLE ktext FROM coas INTO wa_header-ktext2
               WHERE aufnr = konti-aufnr.
            wa_header-aufnr = konti-aufnr.
            SELECT SINGLE ktext FROM cskt INTO wa_header-ktext1
              WHERE spras = sy-langu AND
                    kokrs = konti-kokrs AND
                    kostl = konti-kostl.
            wa_header-kostl = konti-kostl.
          WHEN 2.

            SELECT SINGLE ktext FROM coas INTO wa_header-ktext22
     WHERE aufnr = konti-aufnr.
            wa_header-aufnr22 = konti-aufnr.

            SELECT SINGLE ktext FROM cskt INTO wa_header-ktext12
              WHERE spras = sy-langu AND
                    kokrs = konti-kokrs AND
                    kostl = konti-kostl.
            wa_header-kostl12 = konti-kostl.
          WHEN 3.
            SELECT SINGLE ktext FROM coas INTO wa_header-ktext23
               WHERE aufnr = konti-aufnr.
            wa_header-aufnr23 = konti-aufnr.
            SELECT SINGLE ktext FROM cskt INTO wa_header-ktext13
              WHERE spras = sy-langu AND
                    kokrs = konti-kokrs AND
                    kostl = konti-kostl.
            wa_header-kostl13 = konti-kostl.
          WHEN 4.

            SELECT SINGLE ktext FROM coas INTO wa_header-ktext24
               WHERE aufnr = konti-aufnr.
            wa_header-aufnr24 = konti-aufnr.

            SELECT SINGLE ktext FROM cskt INTO wa_header-ktext14
              WHERE spras = sy-langu AND
                    kokrs = konti-kokrs AND
                    kostl = konti-kostl.
            wa_header-kostl14 = konti-kostl.
          WHEN OTHERS.
            EXIT.
        ENDCASE.

      ENDLOOP.

      SELECT SINGLE lifnr FROM lfb1 INTO w_lifnr
        WHERE bukrs = wa_header-bukrs AND
              pernr = wa_header-pernr.

      SELECT SINGLE bankl bankn FROM lfbk
        INTO (wa_header-bankl, wa_header-bankn)
        WHERE lifnr = w_lifnr.

      APPEND wa_header TO t_header.
    ENDLOOP.

  ENDIF.


  SORT: t_t706b5 BY spkzl,
        t_t706b4 BY spkzl,
        t_t706k  BY lgart,
        t_t030   BY komok,
        t_skat   BY ktopl saknr.

  CLEAR: t_item, w_desp, w_saldo.
  REFRESH t_item.
  DATA: w_conta(10).
  LOOP AT beleg INTO header.

    READ TABLE exbel WITH KEY belnr = header-belnr.
    IF sy-subrc = 0.
      CONCATENATE exbel-descr exbel-place INTO t_item-compl SEPARATED BY ' - '.
    ENDIF.

    READ TABLE t_t706b5 INTO wa_t706b5 WITH KEY spkzl = header-spkzl
                                                BINARY SEARCH.
    IF sy-subrc = 0.
      READ TABLE t_t706b4 INTO wa_t706b4 WITH KEY spkzl = header-spkzl
                                                  BINARY SEARCH.

      IF sy-subrc = 0.
        READ TABLE t_t706k INTO wa_t706k WITH KEY lgart = wa_t706b4-lgarl
                                                    BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE t_t030 INTO wa_t030 WITH KEY komok = wa_t706k-komok
                                                 BINARY SEARCH.
          IF sy-subrc = 0.
            READ TABLE t_skat WITH KEY ktopl = wa_t030-ktopl
                                       saknr = wa_t030-konts
                                       BINARY SEARCH.
            IF sy-subrc = 0.

              WRITE wa_t030-konts TO w_conta.
              CONCATENATE w_conta t_skat-txt20
                          INTO t_item-clas_cont SEPARATED BY '-'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.


*    WRITE:/ HEADER-BELNR,
*            HEADER-SPKZL,
*            HEADER-BETRG,
*            HEADER-BLDAT,
*            HEADER-WAERS,
*            HEADER-KURSB.
    MOVE: header-bldat TO t_item-bldat,
          header-betrg TO t_item-valor,
          header-waers TO t_item-moeda.
    ADD t_item-valor TO w_desp.
    APPEND t_item. CLEAR t_item.
  ENDLOOP.

ENDFORM.                    " F_PROCESSA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_SAPSCRIPT
*&---------------------------------------------------------------------*
FORM f_sapscript .

  w_saldo = wa_header-wrbtr - w_desp.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname = 'ZFITV001'
    IMPORTING
      fm_name  = vg_fm_name
    EXCEPTIONS
      OTHERS   = 3.

*  IF w_saldo < 0.
*    w_saldoe = w_saldo * ( - 1 ).
*  ENDIF.

  IF w_desp < wa_header-wrbtr.
    w_saldoe = w_saldo.
    w_saldo = w_saldo * ( - 1 ).
  ELSE.
    w_saldo = w_saldo * ( - 1 ).
  ENDIF.

  CALL FUNCTION vg_fm_name
    EXPORTING
      w_viagem  = wa_header-reinr
      w_bukrs   = wa_header-bukrs
      w_butxt   = wa_header-butxt
      w_gsber   = wa_header-gsber
      w_gtext   = wa_header-gtext
      w_pernr   = wa_header-pernr
      w_ename   = wa_header-ename
      w_data_b  = wa_header-date_beg
      w_data_e  = wa_header-date_end
      w_local   = wa_header-location_end
      w_reason  = wa_header-request_reason
      w_kostl   = wa_header-kostl
      w_ktext1  = wa_header-ktext1
      w_aufnr   = wa_header-aufnr
      w_ktext2  = wa_header-ktext2
      w_bankl   = wa_header-bankl
      w_bankn   = wa_header-bankn
      w_wrbtr   = wa_header-wrbtr
      w_saldo   = w_saldo
      w_saldoe  = w_saldoe
      w_desp    = w_desp
      w_kostl12 = wa_header-kostl12
      w_ktext12 = wa_header-ktext12
      w_aufnr22 = wa_header-aufnr22
      w_ktext22 = wa_header-ktext22
      w_kostl13 = wa_header-kostl13
      w_ktext13 = wa_header-ktext13
      w_aufnr23 = wa_header-aufnr23
      w_ktext23 = wa_header-ktext23
      w_kostl14 = wa_header-kostl14
      w_ktext14 = wa_header-ktext14
      w_aufnr24 = wa_header-aufnr24
      w_ktext24 = wa_header-ktext24
    TABLES
      t_item    = t_item.

ENDFORM.                    " F_SAPSCRIPT
