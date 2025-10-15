************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 16.06.2008                                          *
* Tipo de prg ...: executável                                          *
* Objetivo    ...: Processamento de dados para DRE                     *
*                                                                      *
************************************************************************

report  zgl003_dre_dados no standard page heading    "Não exibe cabeçalho standard
                         line-size 076               "Comprimento da Linha
                         line-count 65               "Número de Linhas.
                         message-id z01.

constants: c_path type c length 200 value '/usr/interfaces/dre/'.

*----------------------------------------------------------------------*
* Tabelas Transparentes                                                *
*----------------------------------------------------------------------*
tables bkpf.

*----------------------------------------------------------------------*
* Tabelas Internas Globais                                             *
*----------------------------------------------------------------------*
data: begin of wa_dre_003.
        include structure zgl003_dre_est.
data: end   of wa_dre_003.

data: begin of wa_dre_004.
        include structure zgl004_dre_est.
data: end   of wa_dre_004.

data: begin of wa_dre_005.
        include structure zgl005_dre_dados.
data: end   of wa_dre_005.

data: begin of wa_dre_006.
        include structure zgl006_dre_dados.
data: end   of wa_dre_006.

data: begin of wa_dre_007.
        include structure zgl007_dre_dados.
data: end   of wa_dre_007.

data: begin of wa_bkpf,
        bukrs            like bkpf-bukrs,
        gjahr            like bkpf-gjahr,
        belnr            like bkpf-belnr,
        waers            like bkpf-waers,
        bstat            like bkpf-bstat,
        tcode            like bkpf-tcode,
        awkey            like bkpf-awkey,
        awk10            like bkpf-belnr,
        refbk            like cobk-refbk,
        refgj            like cobk-refgj,
        ldgrp            like bkpf-ldgrp,
        xblnr            like bkpf-xblnr,
        awtyp            like bkpf-awtyp,
        blart            like bkpf-blart, " Conforme chamado 57143
        stblg            like bkpf-stblg,
      end   of wa_bkpf,

      begin of wa_bseg,
        bukrs            like bseg-bukrs,
        gjahr            like bseg-gjahr,
        belnr            like bseg-belnr,
        hkont            like bseg-hkont,
        kostl            like bseg-kostl,
        prctr            like bseg-prctr,
        aufnr            like bseg-aufnr,
        wrbtr            like bseg-wrbtr,
        dmbtr            like bseg-dmbtr,
        shkzg            like bseg-shkzg,
        dmbe2            like bseg-dmbt2,
        buzei            like bseg-buzei,
        pswsl            like bseg-pswsl,
        prctr2           like bseg-prctr,
        matnr            like bseg-matnr,
        kokrs            like bseg-kokrs,
      end   of wa_bseg,

      begin of wa_qtde,
        bukrs            like zgl006_dre_dados-bukrs,
        monat            like zgl005_dre_dados-monat,
        gjarh            like zgl005_dre_dados-gjahr,
        saknr            like zgl006_dre_dados-saknr,
        kostl            like zgl006_dre_dados-kostl,
        aufnr            like zgl006_dre_dados-aufnr,
        prctr            like zgl006_dre_dados-prctr,
        qtd_ton          like zgl006_dre_dados-qtd_ton,
        sg_unidade       type c length 7,
        docnum           type j_1bdocnum,
        itmnum           type j_1bitmnum,
      end   of wa_qtde,

      begin of wa_arq,
        bukrs(04),
        monat(02),
        gjarh(04),
        saknr(10),
        kostl(10),
        aufnr(10),
        prctr(10),
        qtd_ton(16),
        sg_unidade(7),
      end   of wa_arq,

      begin of wa_cobk,
        kokrs            like cobk-kokrs,
        perab            like cobk-perab,
        gjahr            like cobk-gjahr,
        belnr            like cobk-belnr,
        refbn            like cobk-refbn,
        refbk            like cobk-refbk,
        refgj            like cobk-refgj,
      end of wa_cobk,

      begin of wa_coep,
        kokrs            like coep-kokrs,
        belnr            like coep-belnr,
        objnr            like coep-objnr,
        parob1           like coep-parob1,
        wtgbtr           like coep-wtgbtr,
        wogbtr           like coep-wogbtr,
        kstar            like coep-kstar,
        twaer            like coep-twaer,
        okostl           like zgl006_dre_dados-kostl,
        pkostl           like zgl006_dre_dados-kostl,
        vrgng            like coep-vrgng,
        beknz            like coep-beknz,
      end of wa_coep,

      begin of wa_coep2,
        kokrs            like coep-kokrs,
        belnr            like coep-belnr,
        objnr            like coep-objnr,
        parob1           like coep-parob1,
        wtgbtr           like coep-wtgbtr,
        wogbtr           like coep-wogbtr,
        kstar            like coep-kstar,
        twaer            like coep-twaer,
        okostl           like zgl006_dre_dados-kostl,
        pkostl           like zgl006_dre_dados-kostl,
      end of wa_coep2,

      begin of wa_coas,
        aufnr            like coas-aufnr,
        auart            like coas-auart,
      end of wa_coas,

      begin of wa_zgl001,
        vg_moeda  like zgl001_dre_est-waers,
        vg_funcao like zgl001_dre_est-waers,
      end of wa_zgl001.

data: begin of wa_arqtxt,
      bukrs(4),
      monat(2),
      gjarh(4),
      saknr(10),
      kostl(10),
      aufnr(12),
      prctr(10),
      qtd_ton(18),
      sg_unidade(7),
      end of wa_arqtxt.

data: begin of wa_zgl013_espera.
        include structure zgl013_espera.
data: end of wa_zgl013_espera.

data: wa_planilha        type alsmex_tabline,  "Work Area p/ Planilha.
      wa_arqerro         like wa_planilha,
      it_dre_003         like standard table of wa_dre_003,
      it_dre_004         like standard table of wa_dre_004,
      it_dre_005         like standard table of wa_dre_005,
      it_dre_006         like standard table of wa_dre_006,
      it_dre_007         like standard table of wa_dre_007,
      it_bkpf            like standard table of wa_bkpf,
      it_bkpf2           like standard table of wa_bkpf,
      it_bkpf3           like standard table of wa_bkpf,
      it_bseg            like standard table of wa_bseg,
      it_bseg2           like standard table of wa_bseg,
      it_qtde            like standard table of wa_qtde,
      it_planilha        like standard table of wa_planilha,
      it_arqerro         like standard table of wa_arqerro,
      it_arq             like standard table of wa_arq,
      it_cobk            like standard table of wa_cobk,
      it_coep            like standard table of wa_coep,
      it_coas            like standard table of wa_coas,
      vg_chave           type c length 14,
      vg_chave_ant       type c length 14,
      vg_mes             type n length 02,
      vg_moeda           like zgl001_dre_est-waers,
      vg_funcao          like zgl001_dre_est-funcao,
      vg_begin_col       type i value 1,
      vg_begin_row       type i value 1,
      vg_tabix           like sy-tabix,
      vg_dia             like sy-datum,
      vg_gdatu           type c length 8,
      it_arqtxt          like standard table of wa_arqtxt,
      tipo_processamento type c length 1,
      r_monat            like range of zgl005_dre_dados-monat with header line.

*----------------------------------------------------------------------*
* Constantes                                                           *
*----------------------------------------------------------------------*
constants: c_end_col      type i value 60,
           c_lines        type i value 64000,
           c_mark         type c value 'X',
           c_n            type c value 'N'.

*----------------------------------------------------------------------*
* Tela de seleção
*----------------------------------------------------------------------*
selection-screen begin of block b4 with frame title text-s04.
parameter:
          r_normal       radiobutton group tp default 'X',
          r_parcia       radiobutton group tp.
selection-screen end   of block b4.

selection-screen begin of block b0 with frame title text-s01.
parameters:
          p_bukrs       like zgl005_dre_dados-bukrs obligatory,
          p_versn       like zgl005_dre_dados-versn matchcode object zversn_dre obligatory,
          p_monat       like zgl005_dre_dados-monat obligatory,
          p_gjahr       like zgl005_dre_dados-gjahr obligatory,
          p_ukurs       like zgl005_dre_dados-ukurs.
selection-screen end   of block b0.

selection-screen begin of block b2 with frame title text-s02.
parameters:
          p_ptch        like rlgrap-filename modif id pat,  "Arq APLIC.SERVER
          p_arq         like rlgrap-filename.  "Arq APLIC.SERVER
selection-screen end   of block b2.

selection-screen begin of block b3 with frame title text-s03.
select-options p_docume for bkpf-belnr.
selection-screen end   of block b3.

*----------------------------------------------------------------------*
* At Selection-Screen                                                  *
*----------------------------------------------------------------------*
at selection-screen on value-request for p_arq.
  call function 'WS_FILENAME_GET'
    exporting
      def_path         = p_arq
      mask             = '*.*'
      mode             = 'O'
      title            = 'Diretório do arquivo de Entrada'
    importing
      filename         = p_arq
    exceptions
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      others           = 5.

at selection-screen on p_gjahr.

  if p_monat is not initial and p_gjahr is not initial and sy-ucomm eq 'EXEC'.
    perform pesquisa_taxa_dolar.
  endif.

at selection-screen.

*  CASE SY-UCOMM.
*    WHEN 'EXIT'.
*      LEAVE PROGRAM.
*    WHEN 'CANC'.
*      LEAVE PROGRAM.
*    WHEN 'BACK'.
*      LEAVE PROGRAM.
*    WHEN 'PPARC'.
*      PERFORM F_VERIFICA_TELA.
*      PERFORM F_PROCESSAMENTO_PARCIAL.
*    WHEN 'EXEC'.
*      PERFORM F_PROCESSAMENTO_COMPLETO.
*    WHEN 'BACKG'.
*      PERFORM F_VERIFICA_TELA.
*      MESSAGE I000 WITH 'Esta função ainda não está ativa!'.
*  ENDCASE.

start-of-selection.

  data : msg_usu type string.

  select single * into wa_zgl013_espera
    from zgl013_espera.

  while sy-subrc eq 0.
    concatenate 'Usuário:' wa_zgl013_espera-uname 'em processamento de DRE' into msg_usu separated by space.
    perform f_mensagem using msg_usu.

    call function 'ENQUE_SLEEP'
      exporting
        seconds = 10.

    select single * into wa_zgl013_espera
      from zgl013_espera.
  endwhile.

  wa_zgl013_espera-mandt    = sy-mandt.
  wa_zgl013_espera-liberado = 'N'.
  wa_zgl013_espera-uname    = sy-uname.
  wa_zgl013_espera-uzeit    = sy-uzeit.
  insert into zgl013_espera values wa_zgl013_espera.

  if r_normal is initial.
    perform f_processamento_parcial.
  else.
    perform f_processamento_completo.
  endif.

  delete from zgl013_espera.


*----------------------------------------------------------------------*
* Initialization                                                       *
*----------------------------------------------------------------------*
initialization.
  set titlebar 'TITULO'.
*  SET PF-STATUS 'TELA_1000'.
  p_ptch = c_path.

*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_DADOS
*&---------------------------------------------------------------------*
*       Busco as informações de estrutura e de dados para a DRE
*----------------------------------------------------------------------*
form f_busca_dados .

  data: it_custo     type table of coep initial size 0 with header line,
        wa_custo     type coep.
  "vg_xreversal LIKE bkpf-xreversal.

  perform f_mensagem using 'Buscando valores para o DRE...'.

  select single waers funcao
    from zgl001_dre_est
    into wa_zgl001
   where bukrs eq p_bukrs
     and versn eq p_versn.

  vg_moeda  = wa_zgl001-vg_moeda.
  vg_funcao = wa_zgl001-vg_funcao.

  select *
    from zgl003_dre_est
    into table it_dre_003
   where bukrs eq p_bukrs
     and versn eq p_versn.

  select *
    from zgl004_dre_est
    into table it_dre_004
   where bukrs eq p_bukrs
     and versn eq p_versn.

  select *
    from zgl007_dre_dados
    into table it_dre_007
   where chave eq vg_chave_ant.

  sort it_dre_007 by chave nivel saknr kostl aufnr prctr.

  if ( it_dre_003[] is initial ) and ( it_dre_004[] is initial ).
    delete from zgl013_espera.
    message i000 with 'Não existe estrutura para esta seleção'.
    stop.
  endif.

  if p_monat ne '12'.

    select bukrs gjahr belnr waers bstat tcode awkey ldgrp xblnr awtyp blart stblg
      from bkpf
      into corresponding fields of table it_bkpf3
     where bukrs eq p_bukrs
       and gjahr eq p_gjahr
       and monat eq p_monat
       and bstat ne 'S'
       and tcode ne 'FBD1'.
    "AND xreversal EQ vg_xreversal.

  else.

    select bukrs gjahr belnr waers bstat tcode awkey ldgrp xblnr awtyp blart stblg
      from bkpf
      into corresponding fields of table it_bkpf3
     where bukrs eq p_bukrs
       and gjahr eq p_gjahr
       and monat in ('12','13','14','15')
       and bstat ne 'S'
       and tcode ne 'FBD1'.
    "AND xreversal EQ vg_xreversal.

  endif.

  check sy-subrc eq 0.

  loop at it_bkpf3 into wa_bkpf.
    if wa_bkpf-awkey is not initial.
      wa_bkpf-awk10 = wa_bkpf-awkey(10).
      wa_bkpf-refbk = wa_bkpf-awkey+10(4).
      wa_bkpf-refgj = wa_bkpf-awkey+14(4).
    endif.
    append wa_bkpf to it_bkpf2.
  endloop.

  if p_monat ne '12'.

    select kokrs perab gjahr belnr refbn refbk refgj
      from cobk
      into table it_cobk
     where kokrs eq 'MAGI'
       and perab eq p_monat
       and gjahr eq p_gjahr
       and stokz ne 'X'
       and stflg ne 'X'
       and refbk eq p_bukrs.
  else.

    select kokrs perab gjahr belnr refbn refbk refgj
      from cobk
      into table it_cobk
     where kokrs eq 'MAGI'
       and perab in ('12','13','14','15')
       and gjahr eq p_gjahr
       and stokz ne 'X'
       and stflg ne 'X'
       and refbk eq p_bukrs.
  endif.

  data : it_coep2 like standard table of wa_coep.
  data : wa_leitura(1).

  sort it_bkpf2 by awk10 tcode.

  loop at it_cobk into wa_cobk.

    wa_leitura = 'S'.
    read table it_bkpf2 into wa_bkpf with key awk10 = wa_cobk-belnr
                                              tcode = 'KB11N'.
    if sy-subrc eq 0.
      wa_leitura = 'N'.
    endif.

*    READ TABLE it_bkpf2 INTO wa_bkpf WITH KEY awk10 = wa_cobk-refbn
*                                              tcode = 'KO88'.
*    IF sy-subrc EQ 0.
*      "Documento de Liquidaçao
*      IF wa_bkpf-awtyp NE 'AUAK'.
*        wa_leitura = 'N'.
*      ENDIF.
*    ENDIF.

    read table it_bkpf2 into wa_bkpf with key awk10 = wa_cobk-refbn
                                              refbk = wa_cobk-kokrs.
    if sy-subrc eq 0.
      wa_leitura = 'N'.
    endif.

    read table it_bkpf2 into wa_bkpf with key awk10 = wa_cobk-refbn
                                              refbk = wa_cobk-refbk
                                              refgj = wa_cobk-refgj
                                              tcode = 'FBD5'.
    if sy-subrc eq 0.
      wa_leitura = 'N'.
    endif.

    if wa_leitura eq 'S'.

      clear: it_coep2.

      select objnr parob1 wtgbtr wogbtr kstar twaer vrgng beknz
        from coep
        into corresponding fields of table it_coep2
       where kokrs eq wa_cobk-kokrs
         and belnr eq wa_cobk-belnr.

      loop at it_coep2 into wa_coep.

        call function 'CONVERSION_EXIT_BEKNZ_OUTPUT'
          exporting
            input  = wa_coep-beknz
          importing
            output = wa_coep-beknz.

        if ( wa_coep-parob1 is not initial ) and
           ( wa_coep-beknz eq 'C' ) and
           ( ( wa_coep-vrgng eq 'RKU1' ) or ( wa_coep-vrgng eq 'RKIV' ) ).
          check wa_coep-objnr(5) eq 'KSMAG'.

          wa_leitura = 'N'.
          wa_coep-okostl = wa_coep-objnr+6.
          wa_coep-pkostl = wa_coep-parob1+6.

          perform kostl_valida using wa_coep-okostl wa_coep-pkostl.
          read table it_bkpf2 into wa_bkpf with key awk10 = wa_cobk-belnr
                                                    tcode = 'KSV5'.
          if sy-subrc eq 0.
            clear: it_bseg.
            DATA ETL506C12R5224 TYPE TABLE OF BSEG.
DATA LT_FIELDS_L506C12R1409 TYPE FAGL_T_FIELD.
LT_FIELDS_L506C12R1409 = VALUE #( ( LINE = 'BUKRS' )
 ( LINE = 'GJAHR' )
 ( LINE = 'BELNR' )
 ( LINE = 'HKONT' )
 ( LINE = 'KOSTL' )
 ( LINE = 'PRCTR' )
 ( LINE = 'AUFNR' )
 ( LINE = 'WRBTR' )
 ( LINE = 'DMBTR' )
 ( LINE = 'SHKZG' )
 ( LINE = 'DMBE2' )
 ( LINE = 'BUZEI' )
 ( LINE = 'PSWSL' )
 ).
DATA RLDNR_L506C12R9942 TYPE RLDNR.
CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
  IMPORTING E_RLDNR = RLDNR_L506C12R9942
  EXCEPTIONS NOT_FOUND     = 1
             MORE_THAN_ONE = 2.
IF SY-SUBRC = 0.
CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
  EXPORTING
    I_RLDNR = RLDNR_L506C12R9942
    I_BUKRS = WA_BKPF-BUKRS
    I_BELNR = WA_BKPF-BELNR
    I_GJAHR = WA_BKPF-GJAHR
    IT_FIELDLIST = LT_FIELDS_L506C12R1409
    IT_WHERE_CLAUSE = VALUE TT_RSDSWHERE( ( |KOSTL EQ { CL_ABAP_DYN_PRG=>QUOTE( WA_COEP-OKOSTL ) }| ) ( | AND | ) ( |HKONT EQ { CL_ABAP_DYN_PRG=>QUOTE( WA_COEP-KSTAR ) }| ) )
  IMPORTING
    ET_BSEG = ETL506C12R5224
  EXCEPTIONS NOT_FOUND = 1.
ENDIF.
IF SY-SUBRC = 0 AND LINES( ETL506C12R5224 ) > 0.
  CLEAR IT_BSEG.
  TYPES: BEGIN OF TYL506C12R5863,
    BUKRS TYPE BSEG-BUKRS,
    GJAHR TYPE BSEG-GJAHR,
    BELNR TYPE BSEG-BELNR,
    HKONT TYPE BSEG-HKONT,
    KOSTL TYPE BSEG-KOSTL,
    PRCTR TYPE BSEG-PRCTR,
    AUFNR TYPE BSEG-AUFNR,
    WRBTR TYPE BSEG-WRBTR,
    DMBTR TYPE BSEG-DMBTR,
    SHKZG TYPE BSEG-SHKZG,
    DMBE2 TYPE BSEG-DMBE2,
    BUZEI TYPE BSEG-BUZEI,
    PSWSL TYPE BSEG-PSWSL,
  END OF TYL506C12R5863.
  DATA: LML506C12R7981 TYPE TYL506C12R5863,
        LWL506C12R701 LIKE LINE OF IT_BSEG.
  LOOP AT ETL506C12R5224 REFERENCE INTO DATA(LDRL506C12R8987).
    LML506C12R7981-BUKRS = LDRL506C12R8987->BUKRS.
    LML506C12R7981-GJAHR = LDRL506C12R8987->GJAHR.
    LML506C12R7981-BELNR = LDRL506C12R8987->BELNR.
    LML506C12R7981-HKONT = LDRL506C12R8987->HKONT.
    LML506C12R7981-KOSTL = LDRL506C12R8987->KOSTL.
    LML506C12R7981-PRCTR = LDRL506C12R8987->PRCTR.
    LML506C12R7981-AUFNR = LDRL506C12R8987->AUFNR.
    LML506C12R7981-WRBTR = LDRL506C12R8987->WRBTR.
    LML506C12R7981-DMBTR = LDRL506C12R8987->DMBTR.
    LML506C12R7981-SHKZG = LDRL506C12R8987->SHKZG.
    LML506C12R7981-DMBE2 = LDRL506C12R8987->DMBE2.
    LML506C12R7981-BUZEI = LDRL506C12R8987->BUZEI.
    LML506C12R7981-PSWSL = LDRL506C12R8987->PSWSL.
    LWL506C12R701 = LML506C12R7981.
    APPEND LWL506C12R701 TO IT_BSEG.
  ENDLOOP.
  SY-DBCNT = LINES( ETL506C12R5224 ).
ELSE.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ENDIF.


            if sy-subrc eq 0.
              loop at it_bseg into wa_bseg.
                if ( ( wa_bseg-pswsl eq wa_coep-twaer  ) and
                     ( wa_bseg-dmbtr eq wa_coep-wtgbtr ) ) or
                   ( ( wa_bseg-pswsl ne wa_coep-twaer  ) and
                     ( wa_bseg-dmbtr eq wa_coep-wogbtr ) ).
                  wa_leitura = 'S'.
                endif.
              endloop.
            else.
              wa_leitura = 'S'.
            endif.
          else.
            wa_leitura = 'S'.
          endif.

          if wa_leitura eq 'S'.
            read table it_coep into wa_coep2 with key okostl = wa_coep-okostl
                                                      pkostl = wa_coep-pkostl
                                                      kstar  = wa_coep-kstar
                                                      binary search.
            if sy-subrc eq 0.
              wa_coep-wtgbtr = wa_coep-wtgbtr + wa_coep2-wtgbtr.
              wa_coep-wogbtr = wa_coep-wogbtr + wa_coep2-wogbtr.
              modify table it_coep from wa_coep.
            else.
              append wa_coep to it_coep.
            endif.
          endif.

          clear: wa_coep.
        endif.
      endloop.
    endif.
  endloop.

  loop at it_bkpf2 into wa_bkpf.
    if ( wa_bkpf-awtyp ne 'COBK' ).
      append wa_bkpf to it_bkpf.
    elseif ( wa_bkpf-awtyp eq 'COBK' ).

      select *
        into corresponding fields of table it_custo
        from coep
       where kokrs eq 'MAGI'
         and belnr eq wa_bkpf-awk10
         and objnr like 'OR%'.

      if sy-subrc ne 0.
        append wa_bkpf to it_bkpf.
      endif.

    endif.
  endloop.

  clear: it_bseg, it_bseg2.

  if vg_funcao eq 'G'.

    select bukrs gjahr belnr hkont kostl
           prctr aufnr wrbtr dmbtr shkzg
           dmbe2 buzei pswsl
      from bseg_add
      into table it_bseg2
       for all entries in it_bkpf
     where bukrs eq it_bkpf-bukrs
       and gjahr eq it_bkpf-gjahr
       and belnr eq it_bkpf-belnr.

    loop at it_bseg2 into wa_bseg.
      append wa_bseg to it_bseg.
    endloop.

    clear: it_bseg2.

  endif.

  select bukrs gjahr belnr hkont kostl
         prctr aufnr wrbtr dmbtr shkzg
         dmbe2 buzei pswsl
    from bsak
    into table it_bseg2
     for all entries in it_bkpf
   where bukrs eq it_bkpf-bukrs
     and gjahr eq it_bkpf-gjahr
     and belnr eq it_bkpf-belnr.

  loop at it_bseg2 into wa_bseg.
    append wa_bseg to it_bseg.
  endloop.

  clear: it_bseg2.

  select bukrs gjahr belnr hkont kostl
         prctr aufnr wrbtr dmbtr shkzg
         dmbe2 buzei pswsl
    from bsad
    into table it_bseg2
     for all entries in it_bkpf
   where bukrs eq it_bkpf-bukrs
     and gjahr eq it_bkpf-gjahr
     and belnr eq it_bkpf-belnr.

  loop at it_bseg2 into wa_bseg.
    append wa_bseg to it_bseg.
  endloop.

  clear: it_bseg2.

  select bukrs gjahr belnr hkont kostl
         prctr aufnr wrbtr dmbtr shkzg
         dmbe2 buzei pswsl
    from bsis
    into table it_bseg2
     for all entries in it_bkpf
   where bukrs eq it_bkpf-bukrs
     and gjahr eq it_bkpf-gjahr
     and belnr eq it_bkpf-belnr.

  loop at it_bseg2 into wa_bseg.
    if wa_bseg-prctr eq '0000009900'.
      perform depara_material_centro_lucro using wa_bseg.
    endif.
    append wa_bseg to it_bseg.
  endloop.

  clear: it_bseg2.

  select aufnr auart
    from aufk
    into table it_coas
    for all entries in it_bseg
   where aufnr eq it_bseg-aufnr
     and auart eq 'ZSTA'.
*     and hkont eq '0000212000'.

endform.                    " F_BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_INF_GER
*&---------------------------------------------------------------------*
*       Gravo os dados de geração da DRE
*----------------------------------------------------------------------*
form f_grava_inf_ger .
  perform f_mensagem using 'Gravando informações de geração...'.
  clear wa_dre_005.
  wa_dre_005-bukrs = p_bukrs.
  wa_dre_005-versn = p_versn.
  wa_dre_005-monat = p_monat.
  wa_dre_005-gjahr = p_gjahr.
  wa_dre_005-uname = sy-uname.
  wa_dre_005-datum = sy-datum.
  wa_dre_005-uzeit = sy-uzeit.
  wa_dre_005-chave = vg_chave.
  wa_dre_005-ukurs = p_ukurs.
  insert into zgl005_dre_dados values wa_dre_005.
endform.                    " F_GRAVA_INF_GER
*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_DADOS_REL
*&---------------------------------------------------------------------*
*       Gravo valores para o relatório final
*----------------------------------------------------------------------*
form f_grava_dados_rel.

  if vg_funcao ne 'G'.
    perform f_grava_dados_fiscal.
  elseif vg_funcao eq 'G'.
    perform f_grava_dados_gerencial.
  endif.

endform.                    " F_GRAVA_DADOS_REL
*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_DRE
*&---------------------------------------------------------------------*
*       Valido se DRE já gerada para o mes e ano informado caso sim
*       elimido os registros anteriores.
*----------------------------------------------------------------------*
form f_valida_dre.
  data: vl_uname         like sy-uname,
        vl_res           type c length 1,
        vl_msg           type c length 50.
  perform f_mensagem using 'Verificando se a chave de DRE já existe...'.
  select single uname
    from zgl005_dre_dados
    into vl_uname
   where chave eq vg_chave.
  if sy-subrc eq 0.
    vl_msg = 'DRE para'.
    concatenate vl_msg
                p_monat
                '/' p_gjahr
                'gerada por' vl_uname
                into vl_msg separated by space.

    call function 'POPUP_TO_CONFIRM_STEP'
      exporting
        textline1 = vl_msg
        textline2 = 'Deseja excluir e gerar novamente?'
        titel     = 'Atenção!'
      importing
        answer    = vl_res.
    if vl_res eq 'J'.
      perform f_mensagem using 'Excluindo dados já gerados de DRE para a chave...'.
      delete from zgl006_dre_dados where chave eq vg_chave.
      delete from zgl007_dre_dados where chave eq vg_chave.
      delete from zgl005_dre_dados where chave eq vg_chave.
    else.
      concatenate 'Processamente da DRE de '
                  p_monat '/' p_gjahr 'cancelada!'
                  into vl_msg separated by space.
      delete from zgl013_espera.
      message i000 with vl_msg.
      stop.
    endif.
  endif.
endform.                    " F_VALIDA_DRE
*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM
*&---------------------------------------------------------------------*
*       Mensagem de acompanhamento de processo
*----------------------------------------------------------------------*
form f_mensagem  using p_msg.
  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      text = p_msg.
endform.                    " F_MENSAGEM
*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_DADOS_ACUMULADO
*&---------------------------------------------------------------------*
*       Gero os valores acumulados para a DRE
*----------------------------------------------------------------------*
form f_grava_dados_acumulado .

  perform f_mensagem using 'Gravando dados e valores acumulados do relatório DRE...'.

  if tipo_processamento eq 'P'.
    select *
      from zgl007_dre_dados
      into table it_dre_007
     where chave eq vg_chave_ant.
    sort it_dre_007 by chave nivel saknr kostl aufnr prctr.
  endif.

  if vg_funcao ne 'G'.
    perform f_grava_dados_acm_fiscal.
  elseif vg_funcao eq 'G'.
    perform f_grava_dados_acm_gerencial.
  endif.

endform.                    " F_GRAVA_DADOS_ACUMULADO
*&---------------------------------------------------------------------*
*&      Form  F_CARREGA_ARQ
*&---------------------------------------------------------------------*
*       Carrega arquivo xls
*----------------------------------------------------------------------*
form f_carrega_arq .
  data: vl_flg_del       type c,
        wa               like wa_qtde,
        tab              like standard table of wa,
        p_arquivo        like filename-fileextern.

  check p_arq is not initial.

  refresh it_planilha.

  sy-batch = 'X'.

  if ( sy-batch eq 'X' ).
    concatenate p_ptch p_arq into p_arquivo.
    perform carrega_xls_back_ground using p_arquivo.
  else.

    p_arquivo = p_arq.

*  "Caregar os dados de uma planilha em uma tabela interna
    call function 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      exporting
        filename                = p_arquivo
        i_begin_col             = vg_begin_col
        i_begin_row             = vg_begin_row
        i_end_col               = c_end_col
        i_end_row               = c_lines
      tables
        intern                  = it_planilha
      exceptions
        inconsistent_parameters = 1
        upload_ole              = 2
        others                  = 3.

    if sy-subrc <> 0.
      delete from zgl013_espera.
      message e000(z01) with 'Problema ao carregar o arquivo XLS'.
    endif.

    sort it_planilha by row col.

    loop at it_planilha into wa_planilha.

      at new row.
        clear: wa_qtde, vl_flg_del.
      endat.

      if ( wa_planilha-value eq 'DEL' or wa_planilha-row eq '0001' ).
        vl_flg_del = c_mark.
      endif.

      check ( vl_flg_del ne c_mark ).

      case wa_planilha-col.
        when  2.
          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = wa_planilha-value
            importing
              output = wa_qtde-bukrs.
        when  7.
          wa_qtde-monat = wa_planilha-value(2).
          wa_qtde-gjarh = wa_planilha-value+3(4).
        when  8.
          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = wa_planilha-value
            importing
              output = wa_qtde-saknr.
        when  9.
          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = wa_planilha-value
            importing
              output = wa_qtde-kostl.
        when  10.
          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = wa_planilha-value
            importing
              output = wa_qtde-aufnr.
        when  11.
          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = wa_planilha-value
            importing
              output = wa_qtde-prctr.
        when  12.
          call function 'STRING_REPLACE'
            exporting
              pattern    = ','
              substitute = '.'
            changing
              text       = wa_planilha-value.
          wa_qtde-qtd_ton = wa_planilha-value.
        when  13.
          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = wa_planilha-value
            importing
              output = wa_qtde-sg_unidade.
      endcase.
      if ( p_bukrs ne wa_qtde-bukrs ) and ( wa_qtde-bukrs is not initial ).
        message i000 with 'Empresa no arquivo diferente.'.
        exit.
      elseif ( p_monat ne wa_qtde-monat ) and ( wa_qtde-monat is not initial ).
        message i000 with 'Mês no arquivo diferente.'.
        exit.
      elseif ( p_gjahr ne wa_qtde-gjarh ) and ( wa_qtde-gjarh is not initial ).
        message i000 with 'Ano no arquivo deiferente.'.
        exit.
      endif.
      at end of row.
        append wa_qtde to it_qtde.
      endat.

    endloop.
  endif.


endform.                    " F_CARREGA_ARQ
*&---------------------------------------------------------------------*
*&      Form  F_VALIDACOES
*&---------------------------------------------------------------------*
*       Validações iniciais
*----------------------------------------------------------------------*
form f_validacoes .

  refresh r_monat.
  clear r_monat.
  if p_monat eq '12'.
    r_monat-sign = 'I'.
    r_monat-option = 'BT'.
    r_monat-low  = '12'.
    r_monat-high = '16'.
  else.
    r_monat-sign = 'I'.
    r_monat-option = 'BT'.
    r_monat-low  = p_monat.
    r_monat-high = p_monat.
  endif.
  append r_monat.
* Gero chave para processamento
  concatenate p_bukrs
              p_versn
              p_monat
              p_gjahr
              into vg_chave.
  if p_monat gt 1.
    vg_mes = p_monat - 1.
  else.
    vg_mes = p_monat.
  endif.
  concatenate p_bukrs
              p_versn
              vg_mes
              p_gjahr
              into vg_chave_ant.

* Validar taxa dolar ultimo dia do mês
*  IF P_UKURS IS INITIAL.
*    MESSAGE I000 WITH 'Para está data não tem taxa de dólar cadastrada.'.
*    EXIT.
*  ENDIF.

endform.                    " F_VALIDACOES

*&---------------------------------------------------------------------*
*&      Form  KOSTL_VALIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_COEP_OKOSTL  text
*      -->P_WA_COEP_PKOSTL  text
*----------------------------------------------------------------------*
form kostl_valida  using    p_wa_coep_okostl
                            p_wa_coep_pkostl.

  data: qtd type i.
  data: str(20) type c.

  if p_wa_coep_okostl is not initial.
    concatenate '0000000000' p_wa_coep_okostl into str.
    call function 'STRING_LENGTH'
      exporting
        string = str
      importing
        length = qtd.
    qtd = qtd - 10.
    p_wa_coep_okostl = str+qtd.
  endif.

  if p_wa_coep_pkostl is not initial.
    concatenate '0000000000' p_wa_coep_pkostl into str.
    call function 'STRING_LENGTH'
      exporting
        string = str
      importing
        length = qtd.
    qtd = qtd - 10.
    p_wa_coep_pkostl = str+qtd.
  endif.

endform.                    " KOSTL_VALIDA


*&---------------------------------------------------------------------*
*&      Form  CARREGA_XLS_BACK_GROUND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FILENAME text
*----------------------------------------------------------------------*
form carrega_xls_back_ground using p_filename like  filename-fileextern.

  data: wa_table   type string,
        file       type c length 250,
        vl_line    like sy-tabix,
        mode       type xflag,
        acess      type xflag.

  mode  = 'T'.
  acess = 'I'.

  file = p_filename.

  open dataset p_filename for input in text mode
                              encoding default
                              with smart linefeed.

  if ( sy-subrc ne 0 ).
    delete from zgl013_espera.
    message e004(z01) with file acess mode raising open_error.
  endif.

  do.
    read dataset file into wa_table.
    if ( sy-subrc ne 0 ).
      exit.
    endif.
    append wa_table to it_arqtxt.
  enddo.

  close dataset file.

  if ( sy-subrc ne 0 ).
    delete from zgl013_espera.
    message e007(z01) with file acess mode raising close_error.
  endif.

  loop at it_arqtxt into wa_arqtxt.
    clear wa_qtde.

    if not wa_arqtxt-qtd_ton is initial.
      call function 'STRING_REPLACE'
        exporting
          pattern    = ','
          substitute = '.'
        changing
          text       = wa_arqtxt-qtd_ton.
    else.
      wa_arqtxt-qtd_ton = '0'.
    endif.

    move-corresponding wa_arqtxt to wa_qtde.
    append wa_qtde to it_qtde.
  endloop.

endform.                    "CARREGA_XLS_BACK_GROUND

*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_DADOS_FISCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_grava_dados_fiscal .

  data: vl_dmbtr like bseg-dmbtr,
        vl_conv  like bseg-dmbtr,
        qtde_arq like zgl006_dre_dados-qtd_ton,
        vg_subrc type sy-subrc.

  sort: it_dre_003 by nivel saknr,
        it_dre_004 by nivel saknr kostl aufnr prctr,
        it_bkpf    by belnr,
        it_bseg    by hkont kostl prctr aufnr belnr,
        it_qtde    by saknr kostl aufnr prctr,
        it_coas    by aufnr auart.
  perform f_mensagem using 'Gravando dados e valores para relatório DRE...'.
  loop at it_dre_003 into wa_dre_003.
    read table it_dre_004 into wa_dre_004 with key nivel = wa_dre_003-nivel
                                                   saknr = wa_dre_003-saknr
                                                   binary search.
    if sy-subrc eq 0.
      clear: wa_dre_004.
      loop at it_dre_004 into wa_dre_004 where nivel eq wa_dre_003-nivel
                                           and saknr eq wa_dre_003-saknr.
        clear: wa_bseg, vl_dmbtr, wa_dre_006, vl_conv.

        if ( wa_dre_004-kostl is not initial ) and
           ( wa_dre_004-aufnr is initial ) . "Centro de custo
          wa_dre_006-bukrs = wa_dre_003-bukrs.
          wa_dre_006-vresn = wa_dre_003-versn.
          wa_dre_006-nivel = wa_dre_003-nivel.
          wa_dre_006-saknr = wa_dre_003-saknr.
          wa_dre_006-chave = vg_chave.
          wa_dre_006-kostl = wa_dre_004-kostl.
          clear: wa_bseg, vl_dmbtr, wa_qtde, vl_conv.
          loop at it_bseg into wa_bseg where hkont = wa_dre_004-saknr
                                         and kostl = wa_dre_004-kostl.
            if wa_bseg-aufnr is initial.
              if wa_bseg-shkzg eq 'H'.
                wa_bseg-dmbtr = wa_bseg-dmbtr * -1.
                wa_bseg-dmbe2 = wa_bseg-dmbe2 * -1.
              endif.
              if vg_moeda eq 'BRL'.
                vl_dmbtr = vl_dmbtr + wa_bseg-dmbtr.
              elseif vg_moeda eq 'USD'.
                vl_dmbtr = vl_dmbtr + wa_bseg-dmbe2.
                "VL_CONV  = VL_CONV + ( WA_BSEG-DMBTR / P_UKURS ).
              endif.
            else.
              read table it_coas into wa_coas with key aufnr = wa_bseg-aufnr
                                                       auart = 'ZSTA'
                                                       binary search.
              if sy-subrc eq 0.
                if wa_bseg-shkzg eq 'H'.
                  wa_bseg-dmbtr = wa_bseg-dmbtr * -1.
                  wa_bseg-dmbe2 = wa_bseg-dmbe2 * -1.
                endif.
                if vg_moeda eq 'BRL'.
                  vl_dmbtr = vl_dmbtr + wa_bseg-dmbtr.
                elseif vg_moeda eq 'USD'.
                  vl_dmbtr = vl_dmbtr + wa_bseg-dmbe2.
                  "VL_CONV  = VL_CONV + ( WA_BSEG-DMBTR / P_UKURS ).
                endif.
              endif.
            endif.
          endloop.
          wa_dre_006-vlr_rea = vl_dmbtr.
          "WA_DRE_006-VLR_REA_CNV = VL_CONV.

          qtde_arq = 0.
          loop at it_qtde into wa_qtde where saknr = wa_dre_004-saknr
                                         and kostl = wa_dre_004-kostl.
            if wa_qtde-sg_unidade ne 'KG'.
              qtde_arq = qtde_arq + ( wa_qtde-qtd_ton * 1000 ).
            else.
              qtde_arq = qtde_arq + wa_qtde-qtd_ton.
            endif.
          endloop.
          wa_dre_006-qtd_ton = qtde_arq / 1000.

          if ( wa_dre_006-vlr_rea ne 0 ) or ( wa_dre_006-vlr_rea_cnv ne 0 ) or
             ( wa_dre_006-qtd_ton ne 0 ).
            perform p_zgl006_dre_dados using wa_dre_006.
          else.
            read table it_dre_007 into wa_dre_007 with key nivel = wa_dre_006-nivel
                                                           saknr = wa_dre_006-saknr
                                                           kostl = wa_dre_006-kostl
                                                           binary search.
            if sy-subrc eq 0.
              perform p_zgl006_dre_dados using wa_dre_006.
            endif.
          endif.
        elseif wa_dre_004-prctr is not initial. "Centro de lucro
          wa_dre_006-bukrs = wa_dre_003-bukrs.
          wa_dre_006-vresn = wa_dre_003-versn.
          wa_dre_006-nivel = wa_dre_003-nivel.
          wa_dre_006-saknr = wa_dre_003-saknr.
          wa_dre_006-chave = vg_chave.
          wa_dre_006-prctr = wa_dre_004-prctr.
          clear: wa_bseg, vl_dmbtr, vl_conv.
          loop at it_bseg into wa_bseg where hkont = wa_dre_004-saknr
                                         and prctr = wa_dre_004-prctr.
            if wa_bseg-shkzg eq 'H'.
              wa_bseg-dmbtr = wa_bseg-dmbtr * -1.
              wa_bseg-dmbe2 = wa_bseg-dmbe2 * -1.
            endif.
            if vg_moeda eq 'BRL'.
              vl_dmbtr = vl_dmbtr + wa_bseg-dmbtr.
            elseif vg_moeda eq 'USD'.
              vl_dmbtr = vl_dmbtr + wa_bseg-dmbe2.
              "VL_CONV  = VL_CONV + ( WA_BSEG-DMBTR / P_UKURS ).
            endif.

            if wa_bseg-prctr2 is not initial.
              perform busca_qtde_remessa using wa_bseg.
            endif.
          endloop.
          wa_dre_006-vlr_rea = vl_dmbtr.
          "WA_DRE_006-VLR_REA_CNV = VL_CONV.

          qtde_arq = 0.
          loop at it_qtde into wa_qtde where saknr = wa_dre_004-saknr
                                         and prctr = wa_dre_004-prctr.
            if wa_qtde-sg_unidade ne 'KG'.
              qtde_arq = qtde_arq + ( wa_qtde-qtd_ton * 1000 ).
            else.
              qtde_arq = qtde_arq + wa_qtde-qtd_ton.
            endif.
          endloop.
          wa_dre_006-qtd_ton = qtde_arq / 1000.

          if ( wa_dre_006-vlr_rea ne 0 ) or ( wa_dre_006-vlr_rea_cnv ne 0 ) or
             ( wa_dre_006-qtd_ton ne 0 ).
            perform p_zgl006_dre_dados using wa_dre_006.
          else.
            read table it_dre_007 into wa_dre_007 with key nivel = wa_dre_006-nivel
                                                           saknr = wa_dre_006-saknr
                                                           prctr = wa_dre_006-prctr
                                                           binary search.
            if sy-subrc eq 0.
              perform p_zgl006_dre_dados using wa_dre_006.
            endif.
          endif.
        elseif wa_dre_004-aufnr is not initial. "Ordem Interna

          perform verifica_aufnr using vg_subrc wa_dre_004-aufnr.

          if vg_subrc eq 0.
            wa_dre_006-bukrs = wa_dre_003-bukrs.
            wa_dre_006-vresn = wa_dre_003-versn.
            wa_dre_006-nivel = wa_dre_003-nivel.
            wa_dre_006-saknr = wa_dre_003-saknr.
            wa_dre_006-chave = vg_chave.
            wa_dre_006-aufnr = wa_dre_004-aufnr.
            clear: wa_bseg, vl_dmbtr, vl_conv.
            loop at it_bseg into wa_bseg where hkont = wa_dre_004-saknr
                                           and aufnr = wa_dre_004-aufnr.

              if wa_bseg-shkzg eq 'H'.
                wa_bseg-dmbtr = wa_bseg-dmbtr * -1.
                wa_bseg-dmbe2 = wa_bseg-dmbe2 * -1.
              endif.
              if vg_moeda eq 'BRL'.
                vl_dmbtr = vl_dmbtr + wa_bseg-dmbtr.
              elseif vg_moeda eq 'USD'.
                vl_dmbtr = vl_dmbtr + wa_bseg-dmbe2.
                "VL_CONV  = VL_CONV + ( WA_BSEG-DMBTR / P_UKURS ).
              endif.
            endloop.
            wa_dre_006-vlr_rea = vl_dmbtr.
            "WA_DRE_006-VLR_REA_CNV = VL_CONV.

            qtde_arq = 0.
            loop at it_qtde into wa_qtde where saknr = wa_dre_004-saknr
                                           and aufnr = wa_dre_004-aufnr.
              if wa_qtde-sg_unidade ne 'KG'.
                qtde_arq = qtde_arq + ( wa_qtde-qtd_ton * 1000 ).
              else.
                qtde_arq = qtde_arq + wa_qtde-qtd_ton.
              endif.
            endloop.
            wa_dre_006-qtd_ton = qtde_arq / 1000.

            if ( wa_dre_006-vlr_rea ne 0 ) or ( wa_dre_006-vlr_rea_cnv ne 0 ) or
               ( wa_dre_006-qtd_ton ne 0 ).
              perform p_zgl006_dre_dados using wa_dre_006.
            else.
              read table it_dre_007 into wa_dre_007 with key nivel = wa_dre_006-nivel
                                                             saknr = wa_dre_006-saknr
                                                             aufnr = wa_dre_006-aufnr
                                                             binary search.
              if sy-subrc eq 0.
                perform p_zgl006_dre_dados using wa_dre_006.
              endif.
            endif.
          endif.
        elseif ( wa_dre_004-kostl is initial )
           and ( wa_dre_004-aufnr is initial )
           and ( wa_dre_004-prctr is initial ).
          wa_dre_006-bukrs = wa_dre_003-bukrs.
          wa_dre_006-vresn = wa_dre_003-versn.
          wa_dre_006-nivel = wa_dre_003-nivel.
          wa_dre_006-saknr = wa_dre_003-saknr.
          wa_dre_006-chave = vg_chave.
          clear: wa_bseg, vl_dmbtr, vl_conv.
          loop at it_bseg into wa_bseg where hkont = wa_dre_004-saknr.
            if wa_bseg-shkzg eq 'H'.
              wa_bseg-dmbtr = wa_bseg-dmbtr * -1.
              wa_bseg-dmbe2 = wa_bseg-dmbe2 * -1.
            endif.
            if vg_moeda eq 'BRL'.
              vl_dmbtr = vl_dmbtr + wa_bseg-dmbtr.
            elseif vg_moeda eq 'USD'.
              vl_dmbtr = vl_dmbtr + wa_bseg-dmbe2.
              "VL_CONV  = VL_CONV + ( WA_BSEG-DMBTR / P_UKURS ).
            endif.
          endloop.
          wa_dre_006-vlr_rea = vl_dmbtr.
          "WA_DRE_006-VLR_REA_CNV = VL_CONV.

          qtde_arq = 0.
          loop at it_qtde into wa_qtde where saknr = wa_dre_004-saknr.
            if wa_qtde-sg_unidade ne 'KG'.
              qtde_arq = qtde_arq + ( wa_qtde-qtd_ton * 1000 ).
            else.
              qtde_arq = qtde_arq + wa_qtde-qtd_ton.
            endif.
          endloop.
          wa_dre_006-qtd_ton = qtde_arq / 1000.

          if ( wa_dre_006-vlr_rea ne 0 ) or ( wa_dre_006-vlr_rea_cnv ne 0 ) or
             ( wa_dre_006-qtd_ton ne 0 ).
            perform p_zgl006_dre_dados using wa_dre_006.
          else.
            read table it_dre_007 into wa_dre_007 with key nivel = wa_dre_006-nivel
                                                           saknr = wa_dre_006-saknr
                                                           binary search.
            if sy-subrc eq 0.
              perform p_zgl006_dre_dados using wa_dre_006.
            endif.
          endif.
        endif.
      endloop.
    else.
      clear: wa_dre_006.
      wa_dre_006-bukrs = wa_dre_003-bukrs.
      wa_dre_006-vresn = wa_dre_003-versn.
      wa_dre_006-nivel = wa_dre_003-nivel.
      wa_dre_006-saknr = wa_dre_003-saknr.
      wa_dre_006-chave = vg_chave.
      clear: wa_bseg, vl_dmbtr, wa_bkpf, vl_conv.

      loop at it_bseg into wa_bseg where hkont = wa_dre_003-saknr.

        if wa_bseg-aufnr is not initial.
          perform verifica_aufnr using vg_subrc wa_bseg-aufnr.
        else.
          vg_subrc = 0.
        endif.

        if vg_subrc eq 0.

          if wa_bseg-shkzg eq 'H'.
            wa_bseg-dmbtr = wa_bseg-dmbtr * -1.
            wa_bseg-dmbe2 = wa_bseg-dmbe2 * -1.
          endif.
          if vg_moeda eq 'BRL'.
            vl_dmbtr = vl_dmbtr + wa_bseg-dmbtr.
          elseif vg_moeda eq 'USD'.
            vl_dmbtr = vl_dmbtr + wa_bseg-dmbe2.
            "VL_CONV  = VL_CONV + ( WA_BSEG-DMBTR / P_UKURS ).
          endif.
        endif.

      endloop.
      wa_dre_006-vlr_rea = vl_dmbtr.
      "WA_DRE_006-VLR_REA_CNV = VL_CONV.

      qtde_arq = 0.
      loop at it_qtde into wa_qtde where saknr = wa_dre_003-saknr.
        if wa_qtde-sg_unidade ne 'KG'.
          qtde_arq = qtde_arq + ( wa_qtde-qtd_ton * 1000 ).
        else.
          qtde_arq = qtde_arq + wa_qtde-qtd_ton.
        endif.
      endloop.
      wa_dre_006-qtd_ton = qtde_arq / 1000.

      if ( wa_dre_006-vlr_rea ne 0 ) or ( wa_dre_006-vlr_rea_cnv ne 0 ) or
         ( wa_dre_006-qtd_ton ne 0 ).
        perform p_zgl006_dre_dados using wa_dre_006.
      else.
        read table it_dre_007 into wa_dre_007 with key nivel = wa_dre_006-nivel
                                                       saknr = wa_dre_006-saknr
                                                       binary search.
        if sy-subrc eq 0.
          perform p_zgl006_dre_dados using wa_dre_006.
        endif.
      endif.
    endif.
  endloop.

  clear: it_dre_004.

  select *
    from zgl004_dre_est
    into table it_dre_004
   where bukrs eq p_bukrs
     and versn eq p_versn.

  sort: it_dre_004 by saknr kostl.

  select * into table it_dre_006
    from zgl006_dre_dados
   where bukrs eq p_bukrs
     and vresn eq p_versn
     and chave eq vg_chave.

  loop at it_coep into wa_coep.

    if wa_coep-wogbtr lt 0.
      wa_coep-wogbtr = wa_coep-wogbtr * -1.
    endif.

    if wa_coep-wtgbtr lt 0.
      wa_coep-wtgbtr = wa_coep-wtgbtr * -1.
    endif.

************************ Pesquisando
    read table it_dre_006 into wa_dre_006 with key
           bukrs = p_bukrs
           vresn = p_versn
           chave = vg_chave
           saknr = wa_coep-kstar
           kostl = wa_coep-okostl.

    if not sy-subrc is initial.
      read table it_dre_004 into wa_dre_004 with key saknr = wa_coep-kstar
                                                     kostl = wa_coep-okostl
                                                     binary search.
      if sy-subrc eq 0.
        clear: wa_dre_006.
        wa_dre_006-bukrs = p_bukrs.
        wa_dre_006-vresn = p_versn.
        wa_dre_006-chave = vg_chave.
        wa_dre_006-nivel = wa_dre_004-nivel.
        wa_dre_006-saknr = wa_coep-kstar.
        wa_dre_006-kostl = wa_coep-okostl.
        wa_dre_006-vlr_rea = 0.
        append wa_dre_006 to it_dre_006.
      endif.
    endif.

    read table it_dre_006 into wa_dre_006 with key
       bukrs = p_bukrs
       vresn = p_versn
       chave = vg_chave
       saknr = wa_coep-kstar
       kostl = wa_coep-pkostl.

    if not sy-subrc is initial.
      read table it_dre_004 into wa_dre_004 with key saknr = wa_coep-kstar
                                               kostl = wa_coep-pkostl
                                               binary search.
      if sy-subrc eq 0.
        clear: wa_dre_006.
        wa_dre_006-bukrs = p_bukrs.
        wa_dre_006-vresn = p_versn.
        wa_dre_006-chave = vg_chave.
        wa_dre_006-nivel = wa_dre_004-nivel.
        wa_dre_006-saknr = wa_coep-kstar.
        wa_dre_006-kostl = wa_coep-pkostl.
        wa_dre_006-vlr_rea = 0.
        append wa_dre_006 to it_dre_006.
      endif.
    endif.

    read table it_dre_006 into wa_dre_006 with key
       bukrs = p_bukrs
       vresn = p_versn
       chave = vg_chave
       saknr = wa_coep-kstar
       kostl = wa_coep-okostl.

    if sy-subrc is initial.
      if wa_coep-twaer eq 'USD'.
        wa_dre_006-vlr_rea = wa_dre_006-vlr_rea - wa_coep-wogbtr.
      else.
        wa_dre_006-vlr_rea = wa_dre_006-vlr_rea - wa_coep-wtgbtr.
      endif.
    endif.

    read table it_dre_006 into wa_dre_006 with key
       bukrs = p_bukrs
       vresn = p_versn
       chave = vg_chave
       saknr = wa_coep-kstar
       kostl = wa_coep-pkostl.

    if sy-subrc is initial.
      if wa_coep-twaer eq 'USD'.
        wa_dre_006-vlr_rea = wa_dre_006-vlr_rea + wa_coep-wogbtr.
      else.
        wa_dre_006-vlr_rea = wa_dre_006-vlr_rea + wa_coep-wtgbtr.
      endif.
    endif.

  endloop.

  loop at it_dre_006 into wa_dre_006.
    modify zgl006_dre_dados from wa_dre_006.
  endloop.

endform.                    " F_GRAVA_DADOS_FISCAL

*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_DADOS_GERENCIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_grava_dados_gerencial .

  data: vl_dmbtr   like bseg-dmbtr,
        vl_conv    like bseg-dmbtr,
        vl_mov_ger like bseg-dmbtr,
        vl_calc_aj like bseg-dmbtr,
        vl_hist_aj like bseg-dmbtr,
        qtde_arq  like zgl006_dre_dados-qtd_ton,
        vg_ledger like bkpf-ldgrp,
        vg_subrc  type sy-subrc.

  sort: it_dre_003 by nivel saknr,
        it_dre_004 by nivel saknr kostl aufnr prctr,
        it_bkpf    by bukrs gjahr belnr,
        it_bseg    by hkont kostl prctr aufnr belnr,
        it_qtde    by saknr kostl aufnr prctr,
        it_coas    by aufnr auart.

  perform f_mensagem using 'Gravando dados e valores para relatório DRE...'.

  loop at it_dre_003 into wa_dre_003.
    read table it_dre_004 into wa_dre_004 with key nivel = wa_dre_003-nivel
                                                   saknr = wa_dre_003-saknr
                                                   binary search.
    if sy-subrc eq 0.
      clear: wa_dre_004.
      loop at it_dre_004 into wa_dre_004 where nivel eq wa_dre_003-nivel
                                           and saknr eq wa_dre_003-saknr.
        clear: wa_bseg, vl_dmbtr, wa_dre_006, vl_conv.

        if ( wa_dre_004-kostl is not initial ) and
           ( wa_dre_004-aufnr is initial ) . "Centro de custo
          wa_dre_006-bukrs = wa_dre_003-bukrs.
          wa_dre_006-vresn = wa_dre_003-versn.
          wa_dre_006-nivel = wa_dre_003-nivel.
          wa_dre_006-saknr = wa_dre_003-saknr.
          wa_dre_006-chave = vg_chave.
          wa_dre_006-kostl = wa_dre_004-kostl.
          clear: wa_bseg, vl_dmbtr, wa_qtde, vl_conv, vl_calc_aj, vl_hist_aj, vl_mov_ger.
          loop at it_bseg into wa_bseg where hkont = wa_dre_004-saknr
                                         and kostl = wa_dre_004-kostl.

            perform ledger_contabilidade_geral using wa_bseg-bukrs wa_bseg-gjahr wa_bseg-belnr vg_ledger.

            if wa_bseg-aufnr is initial.
              if wa_bseg-shkzg eq 'H'.
                wa_bseg-dmbtr = wa_bseg-dmbtr * -1.
                wa_bseg-dmbe2 = wa_bseg-dmbe2 * -1.
              endif.
              if vg_moeda eq 'USD'.
                if vg_ledger eq 'GE'.
                  vl_mov_ger = vl_mov_ger + wa_bseg-dmbe2.
                else.
                  vl_dmbtr   = vl_dmbtr + wa_bseg-dmbe2.
                endif.
              endif.
            else.
              read table it_coas into wa_coas with key aufnr = wa_bseg-aufnr
                                                       auart = 'ZSTA'
                                                       binary search.
              if sy-subrc eq 0.
                if wa_bseg-shkzg eq 'H'.
                  wa_bseg-dmbtr = wa_bseg-dmbtr * -1.
                  wa_bseg-dmbe2 = wa_bseg-dmbe2 * -1.
                endif.
                if vg_moeda eq 'USD'.
                  if vg_ledger eq 'GE'.
                    vl_mov_ger = vl_mov_ger + wa_bseg-dmbe2.
                  else.
                    vl_dmbtr   = vl_dmbtr + wa_bseg-dmbe2.
                  endif.
                endif.
              endif.
            endif.
          endloop.

          wa_dre_006-vlr_rea     = vl_dmbtr.
          wa_dre_006-vlr_mov_ger = vl_mov_ger.
          wa_dre_006-vlr_calc_aj = vl_dmbtr - vl_mov_ger.

          qtde_arq = 0.
          loop at it_qtde into wa_qtde where saknr = wa_dre_004-saknr
                                         and kostl = wa_dre_004-kostl.
            if wa_qtde-sg_unidade ne 'KG'.
              qtde_arq = qtde_arq + ( wa_qtde-qtd_ton * 1000 ).
            else.
              qtde_arq = qtde_arq + wa_qtde-qtd_ton.
            endif.
          endloop.
          wa_dre_006-qtd_ton = qtde_arq / 1000.

          if ( wa_dre_006-vlr_rea ne 0 ) or
             ( wa_dre_006-qtd_ton ne 0 ) or
             ( wa_dre_006-vlr_mov_ger ne 0 ) or
             ( wa_dre_006-vlr_calc_aj ne 0 ).
            perform p_zgl006_dre_dados using wa_dre_006.
          else.
            read table it_dre_007 into wa_dre_007 with key nivel = wa_dre_006-nivel
                                                           saknr = wa_dre_006-saknr
                                                           kostl = wa_dre_006-kostl
                                                           binary search.
            if sy-subrc eq 0.
              perform p_zgl006_dre_dados using wa_dre_006.
            endif.
          endif.
        elseif wa_dre_004-prctr is not initial. "Centro de lucro
          wa_dre_006-bukrs = wa_dre_003-bukrs.
          wa_dre_006-vresn = wa_dre_003-versn.
          wa_dre_006-nivel = wa_dre_003-nivel.
          wa_dre_006-saknr = wa_dre_003-saknr.
          wa_dre_006-chave = vg_chave.
          wa_dre_006-prctr = wa_dre_004-prctr.
          clear: wa_bseg, vl_dmbtr, vl_conv, vl_calc_aj, vl_hist_aj, vl_mov_ger.
          loop at it_bseg into wa_bseg where hkont = wa_dre_004-saknr
                                         and prctr = wa_dre_004-prctr.
            if wa_bseg-shkzg eq 'H'.
              wa_bseg-dmbtr = wa_bseg-dmbtr * -1.
              wa_bseg-dmbe2 = wa_bseg-dmbe2 * -1.
            endif.
            perform ledger_contabilidade_geral using wa_bseg-bukrs wa_bseg-gjahr wa_bseg-belnr vg_ledger.

            if vg_moeda eq 'USD'.
              if vg_ledger eq 'GE'.
                vl_mov_ger = vl_mov_ger + wa_bseg-dmbe2.
              else.
                vl_dmbtr   = vl_dmbtr + wa_bseg-dmbe2.
              endif.
            endif.

            if wa_bseg-prctr2 is not initial.
              perform busca_qtde_remessa using wa_bseg.
            endif.

          endloop.
          wa_dre_006-vlr_rea     = vl_dmbtr.
          wa_dre_006-vlr_mov_ger = vl_mov_ger.
          wa_dre_006-vlr_calc_aj = vl_dmbtr - vl_mov_ger.

          qtde_arq = 0.
          loop at it_qtde into wa_qtde where saknr = wa_dre_004-saknr
                                         and prctr = wa_dre_004-prctr.
            if wa_qtde-sg_unidade ne 'KG'.
              qtde_arq = qtde_arq + ( wa_qtde-qtd_ton * 1000 ).
            else.
              qtde_arq = qtde_arq + wa_qtde-qtd_ton.
            endif.
          endloop.
          wa_dre_006-qtd_ton = qtde_arq / 1000.

          if ( wa_dre_006-vlr_rea ne 0 ) or
             ( wa_dre_006-qtd_ton ne 0 ) or
             ( wa_dre_006-vlr_mov_ger ne 0 ) or
             ( wa_dre_006-vlr_calc_aj ne 0 ).
            perform p_zgl006_dre_dados using wa_dre_006.
          else.
            read table it_dre_007 into wa_dre_007 with key nivel = wa_dre_006-nivel
                                                           saknr = wa_dre_006-saknr
                                                           prctr = wa_dre_006-prctr
                                                           binary search.
            if sy-subrc eq 0.
              perform p_zgl006_dre_dados using wa_dre_006.
            endif.
          endif.
        elseif wa_dre_004-aufnr is not initial. "Ordem Interna

          perform verifica_aufnr using vg_subrc wa_dre_004-aufnr.

          if vg_subrc eq 0.

            wa_dre_006-bukrs = wa_dre_003-bukrs.
            wa_dre_006-vresn = wa_dre_003-versn.
            wa_dre_006-nivel = wa_dre_003-nivel.
            wa_dre_006-saknr = wa_dre_003-saknr.
            wa_dre_006-chave = vg_chave.
            wa_dre_006-aufnr = wa_dre_004-aufnr.
            clear: wa_bseg, vl_dmbtr, vl_conv, vl_calc_aj, vl_hist_aj, vl_mov_ger.
            loop at it_bseg into wa_bseg where hkont = wa_dre_004-saknr
                                           and aufnr = wa_dre_004-aufnr.

              perform ledger_contabilidade_geral using wa_bseg-bukrs wa_bseg-gjahr wa_bseg-belnr vg_ledger.
              if wa_bseg-shkzg eq 'H'.
                wa_bseg-dmbtr = wa_bseg-dmbtr * -1.
                wa_bseg-dmbe2 = wa_bseg-dmbe2 * -1.
              endif.
              if vg_moeda eq 'USD'.
                if vg_ledger eq 'GE'.
                  vl_mov_ger = vl_mov_ger + wa_bseg-dmbe2.
                else.
                  vl_dmbtr   = vl_dmbtr + wa_bseg-dmbe2.
                endif.
              endif.
            endloop.
            wa_dre_006-vlr_rea     = vl_dmbtr.
            wa_dre_006-vlr_mov_ger = vl_mov_ger.
            wa_dre_006-vlr_calc_aj = vl_dmbtr - vl_mov_ger.

            qtde_arq = 0.
            loop at it_qtde into wa_qtde where saknr = wa_dre_004-saknr
                                           and aufnr = wa_dre_004-aufnr.
              if wa_qtde-sg_unidade ne 'KG'.
                qtde_arq = qtde_arq + ( wa_qtde-qtd_ton * 1000 ).
              else.
                qtde_arq = qtde_arq + wa_qtde-qtd_ton.
              endif.
            endloop.
            wa_dre_006-qtd_ton = qtde_arq / 1000.

            if ( wa_dre_006-vlr_rea ne 0 ) or
               ( wa_dre_006-qtd_ton ne 0 ) or
               ( wa_dre_006-vlr_mov_ger ne 0 ) or
               ( wa_dre_006-vlr_calc_aj ne 0 ).
              perform p_zgl006_dre_dados using wa_dre_006.
            else.
              read table it_dre_007 into wa_dre_007 with key nivel = wa_dre_006-nivel
                                                             saknr = wa_dre_006-saknr
                                                             aufnr = wa_dre_006-aufnr
                                                             binary search.
              if sy-subrc eq 0.
                perform p_zgl006_dre_dados using wa_dre_006.
              endif.
            endif.
          endif.
        elseif ( wa_dre_004-kostl is initial )
           and ( wa_dre_004-aufnr is initial )
           and ( wa_dre_004-prctr is initial ).
          wa_dre_006-bukrs = wa_dre_003-bukrs.
          wa_dre_006-vresn = wa_dre_003-versn.
          wa_dre_006-nivel = wa_dre_003-nivel.
          wa_dre_006-saknr = wa_dre_003-saknr.
          wa_dre_006-chave = vg_chave.
          clear: wa_bseg, vl_dmbtr, vl_conv, vl_calc_aj, vl_hist_aj, vl_mov_ger.
          loop at it_bseg into wa_bseg where hkont = wa_dre_004-saknr.
            perform ledger_contabilidade_geral using wa_bseg-bukrs wa_bseg-gjahr wa_bseg-belnr vg_ledger.
            if wa_bseg-shkzg eq 'H'.
              wa_bseg-dmbtr = wa_bseg-dmbtr * -1.
              wa_bseg-dmbe2 = wa_bseg-dmbe2 * -1.
            endif.
            if vg_moeda eq 'USD'.
              if vg_ledger eq 'GE'.
                vl_mov_ger = vl_mov_ger + wa_bseg-dmbe2.
              else.
                vl_dmbtr   = vl_dmbtr + wa_bseg-dmbe2.
              endif.
            endif.
          endloop.
          wa_dre_006-vlr_rea     = vl_dmbtr.
          wa_dre_006-vlr_mov_ger = vl_mov_ger.
          wa_dre_006-vlr_calc_aj = vl_dmbtr - vl_mov_ger.

          qtde_arq = 0.
          loop at it_qtde into wa_qtde where saknr = wa_dre_004-saknr.
            if wa_qtde-sg_unidade ne 'KG'.
              qtde_arq = qtde_arq + ( wa_qtde-qtd_ton * 1000 ).
            else.
              qtde_arq = qtde_arq + wa_qtde-qtd_ton.
            endif.
          endloop.
          wa_dre_006-qtd_ton = qtde_arq / 1000.

          if ( wa_dre_006-vlr_rea ne 0 ) or
             ( wa_dre_006-qtd_ton ne 0 ) or
             ( wa_dre_006-vlr_mov_ger ne 0 ) or
             ( wa_dre_006-vlr_calc_aj ne 0 ).
            perform p_zgl006_dre_dados using wa_dre_006.
          else.
            read table it_dre_007 into wa_dre_007 with key nivel = wa_dre_006-nivel
                                                           saknr = wa_dre_006-saknr
                                                           binary search.
            if sy-subrc eq 0.
              perform p_zgl006_dre_dados using wa_dre_006.
            endif.
          endif.
        endif.
      endloop.
    else.
      clear: wa_dre_006.
      wa_dre_006-bukrs = wa_dre_003-bukrs.
      wa_dre_006-vresn = wa_dre_003-versn.
      wa_dre_006-nivel = wa_dre_003-nivel.
      wa_dre_006-saknr = wa_dre_003-saknr.
      wa_dre_006-chave = vg_chave.
      clear: wa_bseg, vl_dmbtr, wa_bkpf, vl_conv, vl_calc_aj, vl_hist_aj, vl_mov_ger.
      loop at it_bseg into wa_bseg where hkont = wa_dre_003-saknr.

        perform ledger_contabilidade_geral using wa_bseg-bukrs wa_bseg-gjahr wa_bseg-belnr vg_ledger.
        if wa_bseg-shkzg eq 'H'.
          wa_bseg-dmbtr = wa_bseg-dmbtr * -1.
          wa_bseg-dmbe2 = wa_bseg-dmbe2 * -1.
        endif.
        if vg_moeda eq 'USD'.
          if vg_ledger eq 'GE'.
            vl_mov_ger = vl_mov_ger + wa_bseg-dmbe2.
          else.
            vl_dmbtr   = vl_dmbtr + wa_bseg-dmbe2.
          endif.
        endif.
      endloop.
      wa_dre_006-vlr_rea     = vl_dmbtr.
      wa_dre_006-vlr_mov_ger = vl_mov_ger.
      wa_dre_006-vlr_calc_aj = vl_dmbtr - vl_mov_ger.

      qtde_arq = 0.
      loop at it_qtde into wa_qtde where saknr = wa_dre_003-saknr.
        if wa_qtde-sg_unidade ne 'KG'.
          qtde_arq = qtde_arq + ( wa_qtde-qtd_ton * 1000 ).
        else.
          qtde_arq = qtde_arq + wa_qtde-qtd_ton.
        endif.
      endloop.
      wa_dre_006-qtd_ton = qtde_arq / 1000.

      if ( wa_dre_006-vlr_rea ne 0 ) or
         ( wa_dre_006-qtd_ton ne 0 ) or
         ( wa_dre_006-vlr_mov_ger ne 0 ) or
         ( wa_dre_006-vlr_calc_aj ne 0 ).
        perform p_zgl006_dre_dados using wa_dre_006.
      else.
        read table it_dre_007 into wa_dre_007 with key nivel = wa_dre_006-nivel
                                                       saknr = wa_dre_006-saknr
                                                       binary search.
        if sy-subrc eq 0.
          perform p_zgl006_dre_dados using wa_dre_006.
        endif.
      endif.
    endif.
  endloop.

  clear: it_dre_004.

  select *
    from zgl004_dre_est
    into table it_dre_004
   where bukrs eq p_bukrs
     and versn eq p_versn.

  sort: it_dre_004 by saknr kostl.

  select * into table it_dre_006
    from zgl006_dre_dados
   where bukrs eq p_bukrs
     and vresn eq p_versn
     and chave eq vg_chave.

  loop at it_coep into wa_coep.

    if wa_coep-wogbtr lt 0.
      wa_coep-wogbtr = wa_coep-wogbtr * -1.
    endif.

    if wa_coep-wtgbtr lt 0.
      wa_coep-wtgbtr = wa_coep-wtgbtr * -1.
    endif.

************************ Pesquisando
    read table it_dre_006 into wa_dre_006 with key
           bukrs = p_bukrs
           vresn = p_versn
           chave = vg_chave
           saknr = wa_coep-kstar
           kostl = wa_coep-okostl.

    if not sy-subrc is initial.
      read table it_dre_004 into wa_dre_004 with key saknr = wa_coep-kstar
                                                     kostl = wa_coep-okostl
                                                     binary search.
      if sy-subrc eq 0.
        clear: wa_dre_006.
        wa_dre_006-bukrs = p_bukrs.
        wa_dre_006-vresn = p_versn.
        wa_dre_006-chave = vg_chave.
        wa_dre_006-nivel = wa_dre_004-nivel.
        wa_dre_006-saknr = wa_coep-kstar.
        wa_dre_006-kostl = wa_coep-okostl.
        wa_dre_006-vlr_rea = 0.
        append wa_dre_006 to it_dre_006.
      endif.
    endif.

    read table it_dre_006 into wa_dre_006 with key
       bukrs = p_bukrs
       vresn = p_versn
       chave = vg_chave
       saknr = wa_coep-kstar
       kostl = wa_coep-pkostl.

    if not sy-subrc is initial.
      read table it_dre_004 into wa_dre_004 with key saknr = wa_coep-kstar
                                               kostl = wa_coep-pkostl
                                               binary search.
      if sy-subrc eq 0.
        clear: wa_dre_006.
        wa_dre_006-bukrs = p_bukrs.
        wa_dre_006-vresn = p_versn.
        wa_dre_006-chave = vg_chave.
        wa_dre_006-nivel = wa_dre_004-nivel.
        wa_dre_006-saknr = wa_coep-kstar.
        wa_dre_006-kostl = wa_coep-pkostl.
        wa_dre_006-vlr_rea = 0.
        append wa_dre_006 to it_dre_006.
      endif.
    endif.

    read table it_dre_006 into wa_dre_006 with key
       bukrs = p_bukrs
       vresn = p_versn
       chave = vg_chave
       saknr = wa_coep-kstar
       kostl = wa_coep-okostl.

    if sy-subrc is initial.
      if wa_coep-twaer eq 'USD'.
        wa_dre_006-vlr_rea = wa_dre_006-vlr_rea - wa_coep-wogbtr.
      else.
        wa_dre_006-vlr_rea = wa_dre_006-vlr_rea - wa_coep-wtgbtr.
      endif.
    endif.

    read table it_dre_006 into wa_dre_006 with key
       bukrs = p_bukrs
       vresn = p_versn
       chave = vg_chave
       saknr = wa_coep-kstar
       kostl = wa_coep-pkostl.

    if sy-subrc is initial.
      if wa_coep-twaer eq 'USD'.
        wa_dre_006-vlr_rea = wa_dre_006-vlr_rea + wa_coep-wogbtr.
      else.
        wa_dre_006-vlr_rea = wa_dre_006-vlr_rea + wa_coep-wtgbtr.
      endif.
    endif.

  endloop.

  loop at it_dre_006 into wa_dre_006.
    modify zgl006_dre_dados from wa_dre_006.
  endloop.

endform.                    " F_GRAVA_DADOS_GERENCIAL

*&---------------------------------------------------------------------*
*&      Form  LEDGER_CONTABILIDADE_GERAL
*&---------------------------------------------------------------------*
*       Consulta Ledger da contabilidade gerencial
*----------------------------------------------------------------------*
form ledger_contabilidade_geral  using p_bukrs p_gjahr p_belnr p_vg_ledger.

  read table it_bkpf into wa_bkpf with key bukrs = p_bukrs
                                           gjahr = p_gjahr
                                           belnr = p_belnr.
  if sy-subrc eq 0.
    p_vg_ledger = wa_bkpf-ldgrp.
  else.
    clear p_vg_ledger.
  endif.

endform.                    " LEDGER_CONTABILIDADE_GERAL

*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_DADOS_ACM_FISCAL
*&---------------------------------------------------------------------*
*       Gravar dados aculumados estrutura fiscal
*----------------------------------------------------------------------*

form f_grava_dados_acm_fiscal .

  clear: it_dre_006[].

  select *
    from zgl006_dre_dados
    into table it_dre_006
   where chave eq vg_chave.

  sort: it_dre_006 by chave nivel saknr kostl aufnr prctr.

  loop at it_dre_006 into wa_dre_006.
    if it_dre_007[] is not initial.
      clear wa_dre_007.
      read table it_dre_007 into wa_dre_007 with key chave = vg_chave_ant
                                                     nivel = wa_dre_006-nivel
                                                     saknr = wa_dre_006-saknr
                                                     kostl = wa_dre_006-kostl
                                                     aufnr = wa_dre_006-aufnr
                                                     prctr = wa_dre_006-prctr
                                                     binary search.
      wa_dre_007-bukrs = wa_dre_006-bukrs.
      wa_dre_007-vresn = wa_dre_006-vresn.
      wa_dre_007-nivel = wa_dre_006-nivel.
      wa_dre_007-saknr = wa_dre_006-saknr.
      wa_dre_007-kostl = wa_dre_006-kostl.
      wa_dre_007-aufnr = wa_dre_006-aufnr.
      wa_dre_007-prctr = wa_dre_006-prctr.
      wa_dre_007-chave = vg_chave.
      if sy-subrc eq 0.
        wa_dre_007-vlr_rea = wa_dre_007-vlr_rea + wa_dre_006-vlr_rea.
        wa_dre_007-vlr_rea_cnv = wa_dre_007-vlr_rea_cnv + wa_dre_006-vlr_rea_cnv.
        wa_dre_007-qtd_ton = wa_dre_007-qtd_ton + wa_dre_006-qtd_ton.
      else.
        wa_dre_007-vlr_rea     = wa_dre_006-vlr_rea.
        wa_dre_007-vlr_rea_cnv = wa_dre_006-vlr_rea_cnv.
        wa_dre_007-qtd_ton     = wa_dre_006-qtd_ton.
      endif.
      insert into zgl007_dre_dados values wa_dre_007.
    else.
      insert into zgl007_dre_dados values wa_dre_006.
    endif.
  endloop.

endform.                    " F_GRAVA_DADOS_ACM_FISCAL

*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_DADOS_ACM_GERENCIAL
*&---------------------------------------------------------------------*
*       Gravar dados aculumados estrutura gerencial
*----------------------------------------------------------------------*
form f_grava_dados_acm_gerencial .

  clear: it_dre_006[].

  select *
    from zgl006_dre_dados
    into table it_dre_006
   where chave eq vg_chave.

  sort: it_dre_006 by chave nivel saknr kostl aufnr prctr.

  loop at it_dre_006 into wa_dre_006.
    if it_dre_007[] is not initial.
      clear wa_dre_007.
      read table it_dre_007 into wa_dre_007 with key chave = vg_chave_ant
                                                     nivel = wa_dre_006-nivel
                                                     saknr = wa_dre_006-saknr
                                                     kostl = wa_dre_006-kostl
                                                     aufnr = wa_dre_006-aufnr
                                                     prctr = wa_dre_006-prctr
                                                     binary search.
      wa_dre_007-bukrs = wa_dre_006-bukrs.
      wa_dre_007-vresn = wa_dre_006-vresn.
      wa_dre_007-nivel = wa_dre_006-nivel.
      wa_dre_007-saknr = wa_dre_006-saknr.
      wa_dre_007-kostl = wa_dre_006-kostl.
      wa_dre_007-aufnr = wa_dre_006-aufnr.
      wa_dre_007-prctr = wa_dre_006-prctr.
      wa_dre_007-chave = vg_chave.
      if sy-subrc eq 0.
        wa_dre_007-vlr_rea     = wa_dre_007-vlr_rea + wa_dre_006-vlr_rea.
        wa_dre_007-vlr_rea_cnv = wa_dre_007-vlr_rea_cnv + wa_dre_006-vlr_rea_cnv.
        wa_dre_007-qtd_ton     = wa_dre_007-qtd_ton + wa_dre_006-qtd_ton.
        wa_dre_007-vlr_mov_ger = wa_dre_007-vlr_mov_ger + wa_dre_006-vlr_mov_ger.
        wa_dre_007-vlr_calc_aj = wa_dre_007-vlr_calc_aj + wa_dre_006-vlr_calc_aj.
        wa_dre_007-vlr_hist_aj = wa_dre_007-vlr_hist_aj + wa_dre_006-vlr_hist_aj.
      else.
        wa_dre_007-vlr_rea     = wa_dre_006-vlr_rea.
        wa_dre_007-vlr_rea_cnv = wa_dre_006-vlr_rea_cnv.
        wa_dre_007-qtd_ton     = wa_dre_006-qtd_ton.
        wa_dre_007-vlr_mov_ger = wa_dre_006-vlr_mov_ger.
        wa_dre_007-vlr_calc_aj = wa_dre_006-vlr_calc_aj.
        wa_dre_007-vlr_hist_aj = wa_dre_006-vlr_hist_aj.
      endif.
      insert into zgl007_dre_dados values wa_dre_007.
    else.
      insert into zgl007_dre_dados values wa_dre_006.
    endif.
  endloop.

endform.                    " F_GRAVA_DADOS_ACM_GERENCIAL

*&---------------------------------------------------------------------*
*&      Form  F_PROCESSAMENTO_COMPLETO
*&---------------------------------------------------------------------*
*       Processamento Completo.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
form f_processamento_completo .

  tipo_processamento = 'C'.

  perform f_validacoes.
* Carrega arquivo
  perform f_carrega_arq.
* Checar se DRE já gerada para o mes e ano informado caso sim excluir
  perform f_valida_dre.
* Busco dadod para montar o DRE
  perform f_busca_dados.
* Gravo informações da geração do DRE
  perform f_grava_inf_ger.
* Gravo os dados para o DRE
  perform f_grava_dados_rel.
* Graco os dados acumulados do DRE
  perform f_grava_dados_acumulado.
  message s000 with 'DRE gerado com sucesso!'.

endform.                    " F_PROCESSAMENTO_COMPLETO

*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_DRE_PARCIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_valida_dre_parcial using p_tipo p_zgl005datum p_zgl005cputm.

  data: vl_uname         like sy-uname,
        vl_res           type c length 1,
        vl_msg           type c length 50,

        begin of wa_zgl005,
          uname like zgl005_dre_dados-uname,
          datum like zgl005_dre_dados-datum,
          uzeit like zgl005_dre_dados-uzeit,
        end of wa_zgl005.

  clear: wa_zgl005, p_zgl005datum, p_zgl005cputm, vg_chave_ant, vg_chave.

  perform f_mensagem using 'Verificando ajuste de DRE'.

* Gero chave para processamento
  concatenate p_bukrs p_versn p_monat p_gjahr into vg_chave.

  if p_monat gt 1.
    vg_mes = p_monat - 1.
  else.
    vg_mes = p_monat.
  endif.
  concatenate p_bukrs p_versn vg_mes p_gjahr into vg_chave_ant.

* Seleciona a mesma cotação usanda em processamento anterior
  select single uname datum uzeit
    from zgl005_dre_dados
    into wa_zgl005
   where chave eq vg_chave.

  if sy-subrc eq 0.

    p_tipo = 'P'.
    p_zgl005datum = wa_zgl005-datum.
    p_zgl005cputm = wa_zgl005-uzeit.

* Seleciona a cotação
    select single ukurs
      from zgl005_dre_dados
      into p_ukurs
     where chave eq vg_chave.

    vl_msg = 'DRE para'.
    concatenate vl_msg
                p_monat
                '/' p_gjahr
                'gerada por' wa_zgl005-uname
                into vl_msg separated by space.

    call function 'POPUP_TO_CONFIRM_STEP'
      exporting
        textline1 = vl_msg
        textline2 = 'Deseja ajustar com novos lançamentos?'
        titel     = 'Atenção!'
      importing
        answer    = vl_res.

    if vl_res ne 'J'.
      concatenate 'Ajuste da DRE de '
                  p_monat '/' p_gjahr 'cancelada!'
                  into vl_msg separated by space.
      message i000 with vl_msg.
      stop.
    else.
* Apagar total atual para totalizar novamente no final
      delete from zgl007_dre_dados where chave eq vg_chave.
    endif.

  else.
    p_tipo = 'C'.
  endif.

endform.                    " F_VALIDA_DRE_PARCIAL

*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_AJUSTE_DRE
*&---------------------------------------------------------------------*
*       Grava ajuste de data e hora de DRE
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
form f_grava_ajuste_dre .

  perform f_mensagem using 'Gravando ajustes de geração...'.

  update zgl005_dre_dados
     set datum = sy-datum
         uzeit = sy-uzeit
   where chave eq vg_chave.

endform.                    " F_GRAVA_AJUSTE_DRE

*&---------------------------------------------------------------------*
*&      Form  F_PROCESSAMENTO_PARCIAL
*&---------------------------------------------------------------------*
*       Procedimento de Processamento Parcial.
*----------------------------------------------------------------------*
*       Os registros encontrados fazer o processo atual somente na     *
*       atualização da tabela ZGL006_DRE_DADOS e ZGL007_DRE_DADOS os   *
*       campos tem que ser de valores tem que ser alterados            *
*       adicionando o valor dos novos registros.                       *
*----------------------------------------------------------------------*
form f_processamento_parcial .

  data: p_tipo  type c length 1,
        p_datum like zgl005_dre_dados-datum,
        p_uzeit like zgl005_dre_dados-uzeit.

* Validações parcial
* P - Parcial
* C - Completo --> Pois não existe, nunca foi gerado.
  perform f_valida_dre_parcial using p_tipo p_datum p_uzeit.

  if p_tipo eq 'P'.
    tipo_processamento = 'P'.
* Busco dadod para montar o DRE
    perform f_busca_dados_parcial using p_datum p_uzeit.
* Ajusta data hora de geração da DRE.
    perform f_grava_ajuste_dre.
* Gravo os dados para o DRE
    perform f_grava_dados_rel.
* Graco os dados acumulados do DRE
    perform f_grava_dados_acumulado.
    message s000 with 'DRE ajustada com sucesso!'.
  else.
* Pesquisa taxa de dolar para processamento completo
    perform pesquisa_taxa_dolar.
    perform f_processamento_completo.
  endif.

endform.                    " F_PROCESSAMENTO_PARCIAL


*&---------------------------------------------------------------------*
*&      Form  PESQUISA_TAXA_DOLAR
*&---------------------------------------------------------------------*
*       Procedimento de Busca de taxa de dólar
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
form pesquisa_taxa_dolar .

  concatenate p_gjahr
              p_monat
              '01' into vg_dia.
  call function 'RP_LAST_DAY_OF_MONTHS'
    exporting
      day_in            = vg_dia
    importing
      last_day_of_month = vg_dia.

  concatenate vg_dia+6(2)
              vg_dia+4(2)
              vg_dia(4) into vg_dia.

  call function 'CONVERSION_EXIT_INVDT_INPUT'
    exporting
      input  = vg_dia
    importing
      output = vg_gdatu.

  select single ukurs
    from tcurr
    into p_ukurs
   where kurst eq 'DRE'
     and fcurr eq 'USD'
     and tcurr eq 'BRL'
     and gdatu eq vg_gdatu.

  if sy-subrc ne 0.
    message i000 with 'Para está data não tem taxa de dólar cadastrada.'.
    exit.
  endif.

endform.                    " PESQUISA_TAXA_DOLAR


*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_DADOS_PARCIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DATUM  text
*      -->P_P_UZEIT  text
*----------------------------------------------------------------------*
form f_busca_dados_parcial  using    p_datum
                                     p_uzeit.

  perform f_mensagem using 'Buscando valores para o DRE...'.

  select single waers funcao
    from zgl001_dre_est
    into wa_zgl001
   where bukrs eq p_bukrs
     and versn eq p_versn.

  vg_moeda  = wa_zgl001-vg_moeda.
  vg_funcao = wa_zgl001-vg_funcao.

  select *
    from zgl003_dre_est
    into table it_dre_003
   where bukrs eq p_bukrs
     and versn eq p_versn.

  select *
    from zgl004_dre_est
    into table it_dre_004
   where bukrs eq p_bukrs
     and versn eq p_versn.

  if ( it_dre_003[] is initial ) and ( it_dre_004[] is initial ).
    delete from zgl013_espera.
    message i000 with 'Não existe estrutura para esta seleção'.
    stop.
  endif.

  if p_monat ne '12'.

* Selecionando documentos contábeis do dia a partir da hora
    select bukrs gjahr belnr waers bstat tcode awkey ldgrp xblnr awtyp blart " Adicionado awtyp blart
      from bkpf
      into corresponding fields of table it_bkpf3
     where bukrs eq p_bukrs
       and gjahr eq p_gjahr
       and monat eq p_monat
       and bstat ne 'S'
       and tcode ne 'FBD1'
       and tcode ne 'KO88'
       and cpudt eq p_datum
       and cputm ge p_uzeit.

* Selecionando documentos contábeis após o dia de geração
    select bukrs gjahr belnr waers bstat tcode awkey ldgrp xblnr awtyp blart
      from bkpf
      into corresponding fields of table it_bkpf2
     where bukrs eq p_bukrs
       and gjahr eq p_gjahr
       and monat eq p_monat
       and bstat ne 'S'
       and tcode ne 'FBD1'
       and tcode ne 'KO88'
       and cpudt gt p_datum.

  else.

* Selecionando documentos contábeis do dia a partir da hora
    select bukrs gjahr belnr waers bstat tcode awkey ldgrp xblnr awtyp blart
      from bkpf
      into corresponding fields of table it_bkpf3
     where bukrs eq p_bukrs
       and gjahr eq p_gjahr
       and monat in ('12','13','14','15')
       and bstat ne 'S'
       and tcode ne 'FBD1'
       and tcode ne 'KO88'
       and cpudt eq p_datum
       and cputm ge p_uzeit.

* Selecionando documentos contábeis após o dia de geração
    select bukrs gjahr belnr waers bstat tcode awkey ldgrp xblnr awtyp blart
      from bkpf
      into corresponding fields of table it_bkpf2
     where bukrs eq p_bukrs
       and gjahr eq p_gjahr
       and monat in ('12','13','14','15')
       and bstat ne 'S'
       and tcode ne 'FBD1'
       and tcode ne 'KO88'
       and cpudt gt p_datum.

  endif.

  loop at it_bkpf2 into wa_bkpf.
    append wa_bkpf to it_bkpf3.
  endloop.

  clear: it_bkpf.

  loop at it_bkpf3 into wa_bkpf.
    if wa_bkpf-awkey is not initial.
      wa_bkpf-awk10 = wa_bkpf-awkey(10).
      wa_bkpf-refbk = wa_bkpf-awkey+10(4).
      wa_bkpf-refgj = wa_bkpf-awkey+14(4).
    endif.
    append wa_bkpf to it_bkpf.
  endloop.

  clear: it_bseg, it_bseg2.

  if it_bkpf is not initial.

    if vg_funcao eq 'G'.

      select bukrs gjahr belnr hkont kostl
             prctr aufnr wrbtr dmbtr shkzg
             dmbe2 buzei pswsl
        from bseg_add
        into table it_bseg2
         for all entries in it_bkpf
       where bukrs eq it_bkpf-bukrs
         and gjahr eq it_bkpf-gjahr
         and belnr eq it_bkpf-belnr.

      loop at it_bseg2 into wa_bseg.
        append wa_bseg to it_bseg.
      endloop.

      clear: it_bseg2.

    endif.

    select bukrs gjahr belnr hkont kostl
           prctr aufnr wrbtr dmbtr shkzg
           dmbe2 buzei pswsl
      from bsak
      into table it_bseg2
       for all entries in it_bkpf
     where bukrs eq it_bkpf-bukrs
       and gjahr eq it_bkpf-gjahr
       and belnr eq it_bkpf-belnr.

    loop at it_bseg2 into wa_bseg.
      append wa_bseg to it_bseg.
    endloop.

    clear: it_bseg2.

    select bukrs gjahr belnr hkont kostl
           prctr aufnr wrbtr dmbtr shkzg
           dmbe2 buzei pswsl
      from bsad
      into table it_bseg2
       for all entries in it_bkpf
     where bukrs eq it_bkpf-bukrs
       and gjahr eq it_bkpf-gjahr
       and belnr eq it_bkpf-belnr.

    loop at it_bseg2 into wa_bseg.
      append wa_bseg to it_bseg.
    endloop.

    clear: it_bseg2.

    select bukrs gjahr belnr hkont kostl
           prctr aufnr wrbtr dmbtr shkzg
           dmbe2 buzei pswsl
      from bsis
      into table it_bseg2
       for all entries in it_bkpf
     where bukrs eq it_bkpf-bukrs
       and gjahr eq it_bkpf-gjahr
       and belnr eq it_bkpf-belnr.

    loop at it_bseg2 into wa_bseg.
      if wa_bseg-prctr eq '0000009900'.
        perform depara_material_centro_lucro using wa_bseg.
      endif.
      append wa_bseg to it_bseg.
    endloop.

    clear: it_bseg2.

  endif.

endform.                    " F_BUSCA_DADOS_PARCIAL

*&---------------------------------------------------------------------*
*&      Form  P_ZGL006_DRE_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_DRE_006  text
*----------------------------------------------------------------------*
form p_zgl006_dre_dados  using p_wa_dre_006 like wa_dre_006.

  if tipo_processamento = 'C'.

    insert into zgl006_dre_dados values p_wa_dre_006.

  elseif tipo_processamento = 'P'.

    select single bukrs
      into p_wa_dre_006-bukrs
      from zgl006_dre_dados
     where bukrs eq p_wa_dre_006-bukrs
       and vresn eq p_wa_dre_006-vresn
       and chave eq p_wa_dre_006-chave
       and nivel eq p_wa_dre_006-nivel
       and saknr eq p_wa_dre_006-saknr
       and kostl eq p_wa_dre_006-kostl
       and aufnr eq p_wa_dre_006-aufnr
       and prctr eq p_wa_dre_006-prctr.

    if sy-subrc eq 0.
      update zgl006_dre_dados
         set qtd_ton      = qtd_ton + p_wa_dre_006-qtd_ton
             vlr_rea      = vlr_rea + p_wa_dre_006-vlr_rea
             vlr_rea_cnv  = vlr_rea_cnv + p_wa_dre_006-vlr_rea_cnv
             vlr_mov_ger  = vlr_mov_ger + p_wa_dre_006-vlr_mov_ger
             vlr_calc_aj  = vlr_calc_aj + p_wa_dre_006-vlr_calc_aj
             vlr_hist_aj  = vlr_hist_aj + p_wa_dre_006-vlr_hist_aj
       where bukrs eq p_wa_dre_006-bukrs
         and vresn eq p_wa_dre_006-vresn
         and chave eq p_wa_dre_006-chave
         and nivel eq p_wa_dre_006-nivel
         and saknr eq p_wa_dre_006-saknr
         and kostl eq p_wa_dre_006-kostl
         and aufnr eq p_wa_dre_006-aufnr
         and prctr eq p_wa_dre_006-prctr.
    else.
      insert into zgl006_dre_dados values p_wa_dre_006.
    endif.

  endif.

endform.                    " P_ZGL006_DRE_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_VERIFICA_TELA
*&---------------------------------------------------------------------*
*       Procedimento de validação de tela.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
form f_verifica_tela .

  if p_bukrs is initial.
    perform f_mensagem_obrigatoriedade using 'Deve ser informado a empresa!'.
  elseif p_versn is initial.
    perform f_mensagem_obrigatoriedade using 'Deve ser informada a estrutura da DRE!'.
  elseif p_monat is initial.
    perform f_mensagem_obrigatoriedade using 'Deve ser informado o mês!'.
  elseif p_gjahr is initial.
    perform f_mensagem_obrigatoriedade using 'Deve ser informado o ano!'.
  endif.

endform.                    " F_VERIFICA_TELA

*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_OBRIGATORIEDADE
*&---------------------------------------------------------------------*
*       Mensagem de obrigatoriedade de preenchimento de campo
*----------------------------------------------------------------------*
*      -->P_6621   Texto de visualização do usuário
*----------------------------------------------------------------------*
form f_mensagem_obrigatoriedade  using p_msg.
  message e000 with p_msg.
endform.                    " F_MENSAGEM_OBRIGATORIEDADE

*&---------------------------------------------------------------------*
*&      Form  DEPARA_MATERIAL_CENTRO_LUCRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form depara_material_centro_lucro  using  p_wa_bseg like wa_bseg.

  data: vg_prctr      type prctr,
        wa_bseg2      type bseg,
        vg_objnr      type c length 22,
        vg_paobjnr    type rkeobjnr,
        vg_artnr      type artnr,
        vg_belnr      type co_belnr,
        it_bkpf_wr    like standard table of wa_bkpf,
        it_j_1bnflin  type table of j_1bnflin,
        wa_j_1bnflin  type j_1bnflin,
        vg_refkey     type j_1brefkey,
        vg_docestorno type c length 1,
        it_objnr      type table of lxhme_range_c22,
        wa_objnr      type lxhme_range_c22.

  wa_objnr-sign   = 'I'.
  wa_objnr-option = 'CP'.
  wa_objnr-low    = 'EO*'.
  append wa_objnr to it_objnr.

  clear: vg_docestorno.

  DATA ETL2549C2R6971 TYPE TABLE OF BSEG.
DATA RLDNR_L2549C2R9648 TYPE RLDNR.
CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
  IMPORTING E_RLDNR = RLDNR_L2549C2R9648
  EXCEPTIONS NOT_FOUND     = 1
             MORE_THAN_ONE = 2.
IF SY-SUBRC = 0.
CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
  EXPORTING
    I_RLDNR = RLDNR_L2549C2R9648
    I_BUKRS = P_WA_BSEG-BUKRS
    I_BELNR = P_WA_BSEG-BELNR
    I_GJAHR = P_WA_BSEG-GJAHR
    I_BUZEI = P_WA_BSEG-BUZEI
  IMPORTING
    ET_BSEG = ETL2549C2R6971
  EXCEPTIONS NOT_FOUND = 1.
ENDIF.
IF SY-SUBRC = 0 AND LINES( ETL2549C2R6971 ) = 1.
  WA_BSEG2 = ETL2549C2R6971[ 1 ].
  SY-DBCNT = 1.
ELSE.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ENDIF.


  move it_bkpf[] to it_bkpf_wr[].
  delete it_bkpf_wr where blart ne 'WR'.

  if not wa_bseg2-matnr is initial.

    p_wa_bseg-matnr = wa_bseg2-matnr.
    p_wa_bseg-kokrs = wa_bseg2-kokrs.

    if wa_bseg2-gsber is initial.
      wa_bseg2-gsber = wa_bseg2-werks.
      call function 'Z_CENTRO_REAL_VIRTUAL'
        exporting
          centro               = wa_bseg2-gsber
        importing
          centro_out           = wa_bseg2-gsber
        exceptions
          informar_centro      = 1
          nao_centro_r_virtual = 2
          informar_centro_out  = 3
          informar_centro_v    = 4
          others               = 5.
    endif.

    select single prctr into vg_prctr
      from zgl014_dre_depa
     where bukrs eq wa_bseg2-bukrs
       and gsber eq wa_bseg2-gsber
       and matnr eq wa_bseg2-matnr
       and kokrs eq wa_bseg2-kokrs.

    wa_bseg-prctr2 = wa_bseg-prctr.
    wa_bseg-prctr  = vg_prctr.

  else.

    read table it_bkpf_wr into wa_bkpf with key bukrs = wa_bseg2-bukrs
                                                gjahr = wa_bseg2-gjahr
                                                belnr = wa_bseg2-belnr.

    if ( not wa_bkpf-stblg is initial ) and ( wa_bkpf-blart eq 'WR' ) and ( wa_bkpf-awkey is initial ).
      read table it_bkpf_wr into wa_bkpf with key bukrs = wa_bkpf-bukrs
                                                  gjahr = wa_bkpf-gjahr
                                                  belnr = wa_bkpf-stblg.
      if sy-subrc is initial.
        vg_docestorno = 'X'.
      endif.
    endif.

    if ( sy-subrc is initial ) and ( wa_bkpf-blart eq 'WR' ) and ( ( wa_bkpf-awkey is not initial ) or ( wa_bkpf-xblnr is not initial ) ).

      vg_refkey = wa_bkpf-awkey+3(10).
      select * into table it_j_1bnflin
        from j_1bnflin
       where refkey eq vg_refkey
         and reftyp eq 'ZW'.

      if not sy-subrc is initial.
        read table it_bkpf_wr into wa_bkpf with key bukrs = wa_bseg2-bukrs
                                                    gjahr = wa_bseg2-gjahr
                                                    stblg = wa_bseg2-belnr.
        if sy-subrc is initial.
          vg_refkey = wa_bkpf-awkey+3(10).
          vg_docestorno = 'X'.
          select * into table it_j_1bnflin
            from j_1bnflin
           where refkey eq vg_refkey
             and reftyp eq 'ZW'.
        else.
          vg_belnr = wa_bkpf-awkey+3(10).
          read table it_bkpf_wr into wa_bkpf with key bukrs = wa_bseg2-bukrs
                                                      gjahr = wa_bseg2-gjahr
                                                      belnr = vg_belnr.
          if sy-subrc is initial.
            vg_refkey = wa_bkpf-awkey+3(10).
            vg_docestorno = 'X'.
            select * into table it_j_1bnflin
              from j_1bnflin
             where refkey eq vg_refkey
               and reftyp eq 'ZW'.
          endif.

        endif.

      endif.

      loop at it_j_1bnflin into wa_j_1bnflin.

        wa_bseg2-matnr = wa_j_1bnflin-matnr.

        select single prctr into vg_prctr
          from zgl014_dre_depa
         where bukrs eq wa_bseg2-bukrs
           and gsber eq wa_bseg2-gsber
           and matnr eq wa_bseg2-matnr
           and kokrs eq wa_bseg2-kokrs.

        if sy-subrc is initial.
          wa_bseg-prctr2 = wa_bseg-prctr.
          wa_bseg-prctr  = vg_prctr.

          "Se não for estornado e documento de estorno
          if ( wa_bkpf-stblg is initial ) and ( vg_docestorno is initial ).
            wa_qtde-saknr      = wa_bseg-hkont. " Conta
            wa_qtde-prctr      = wa_bseg-prctr. " Centro de lucro
            wa_qtde-qtd_ton    = wa_j_1bnflin-menge.
            wa_qtde-sg_unidade = wa_j_1bnflin-meins.
            wa_qtde-docnum     = wa_j_1bnflin-docnum.
            wa_qtde-itmnum     = wa_j_1bnflin-itmnum.
            append wa_qtde to it_qtde.
          else.
            delete it_qtde where docnum = wa_j_1bnflin-docnum
                             and itmnum = wa_j_1bnflin-itmnum.
          endif.
        endif.

      endloop.

    else.

      select single belnr
        into vg_belnr
        from cobk
       where kokrs eq 'MAGI'
         and refbn eq p_wa_bseg-belnr
         and gjahr eq p_wa_bseg-gjahr.

      if sy-subrc eq 0.

        select single objnr
          into vg_objnr
          from coep
         where belnr eq vg_belnr
           and kokrs eq 'MAGI'
           and objnr in it_objnr.

        vg_paobjnr = vg_objnr+6(10).

        if sy-subrc eq  0.

          select single artnr
            into vg_artnr
            from ce4magi
           where paobjnr eq vg_paobjnr.

          if sy-subrc eq 0.

            wa_bseg2-matnr  = vg_artnr.
            wa_bseg2-kokrs  = 'MAGI'.
            p_wa_bseg-matnr = wa_bseg2-matnr.
            p_wa_bseg-kokrs = wa_bseg2-kokrs.

            select single prctr into vg_prctr
              from zgl014_dre_depa
             where bukrs eq wa_bseg2-bukrs
               and gsber eq wa_bseg2-gsber
               and matnr eq wa_bseg2-matnr
               and kokrs eq wa_bseg2-kokrs.

            wa_bseg-prctr2 = wa_bseg-prctr.
            wa_bseg-prctr  = vg_prctr.

          endif.

        endif.

      endif.

    endif.

  endif.

endform.                    " DEPARA_MATERIAL_CENTRO_LUCRO

*&---------------------------------------------------------------------*
*&      Form  BUSCA_QTDE_REMESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form busca_qtde_remessa  using  p_bseg like wa_bseg.

  data: vg_vbeln   type vbeln_nach,
        vg_vbelv   type vbeln_von,
        it_lips    type table of lips initial size 0 with header line,
        wa_lips    type lips,
        vg_fksto   type vbrk-fksto,
        v_seq_lcto type zfiwrt0009-seq_lcto,
        v_matnr    type zfiwrt0009-matnr,
        v_menge    type zfiwrt0009-menge,
        v_meins    type zfiwrt0009-meins,
        v_bwkey    type zfiwrt0009-bwkey,
        v_prctr    type zgl014_dre_depa-prctr.


  read table it_bkpf3 into wa_bkpf with key bukrs = p_bseg-bukrs
                                            gjahr = p_bseg-gjahr
                                            belnr = p_bseg-belnr.

  if ( sy-subrc eq 0 ) and ( not wa_bkpf-xblnr is initial ).

    vg_vbeln = wa_bkpf-xblnr(10).

    select single fksto into vg_fksto
      from vbrk
     where vbeln eq vg_vbeln.

    if vg_fksto is initial.

      select single vbelv into vg_vbelv
        from vbfa
       where vbeln   eq vg_vbeln
         and vbtyp_n eq 'M'
         and vbtyp_v eq 'J'.

      if ( sy-subrc eq 0 ) and ( wa_bkpf-blart ne 'WR' ).

        clear: it_lips[].

        select *
          into corresponding fields of table it_lips
          from lips
         where vbeln eq vg_vbelv
           and matnr eq p_bseg-matnr.

        loop at it_lips into wa_lips.
          wa_qtde-saknr      = p_bseg-hkont. " Conta
          wa_qtde-prctr      = p_bseg-prctr. " Centro de lucro
          wa_qtde-qtd_ton    = wa_lips-ntgew.
          wa_qtde-sg_unidade = wa_lips-gewei.
          append wa_qtde to it_qtde.
        endloop.
        "Conforme chamado 57143.
*      ELSEIF ( sy-subrc EQ 0 ) AND ( wa_bkpf-blart EQ 'WR' ).
*
*        v_seq_lcto = wa_bkpf-awkey+2(16).
*
*        SELECT SINGLE  matnr menge meins bwkey
*         FROM zfiwrt0009
*          INTO (v_matnr, v_menge, v_meins, v_bwkey)
*        WHERE seq_lcto EQ v_seq_lcto.
*
*        SELECT SINGLE prctr
*          FROM zgl014_dre_depa
*          INTO v_prctr
*        WHERE bukrs EQ p_bukrs
*          AND gsber EQ v_bwkey
*          AND matnr EQ v_matnr
*          AND kokrs EQ 'MAGI'.
*
*        wa_qtde-saknr      = p_bseg-hkont. " Conta
*        wa_qtde-prctr      = v_prctr.      " Centro de lucro
*        wa_qtde-qtd_ton    = v_menge.      " Quantidade
*        wa_qtde-sg_unidade = v_meins.      " Unidade
*
*        APPEND wa_qtde TO it_qtde.
*        " Fim Alteração chamado 57143
      endif.

    endif.

  endif.

endform.                    " BUSCA_QTDE_REMESSA

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_AUFNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VG_SUBRC  text
*      -->P_WA_DRE_004_AUFNR  text
*----------------------------------------------------------------------*
form verifica_aufnr  using    p_vg_subrc type sy-subrc
                              p_wa_dre_004_aufnr type aufnr.

  data: w_aufk type aufk.

  p_vg_subrc = 0.

  select *
    into w_aufk
    from aufk
   where aufnr eq p_wa_dre_004_aufnr
     and auart eq 'ZRM1'.

    p_vg_subrc = 4.

  endselect.

endform.                    " VERIFICA_AUFNR
