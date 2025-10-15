************************************************************************
*     P R O J E T O  C R E S C E R   -   M A G G I                     *
*                                                                      *
************************************************************************
* Consultoria ...:                                                     *
* Responsável ...: Michely Stefanoski                                  *
* Data desenv ...: 30.10.2007                                          *
* Tipo de prg ...: Report                                              *
* Objetivo    ...: Alterações necessarias para a geração do arquivo    *
*                  magnético do SINTEGRA programa standard j_1blfa1    *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 30.10.2007    Michely              Correções tipo 71    DEVK903025   *
*    Motivo: Não estava gerando o tipo 71,e o nº das notas não estava  *
*            preenchendo com (0)zeros.                                 *
************************************************************************

report zj_1blfa1 message-id 8b line-size 126.
*ENHANCEMENT-POINT J_1BLFA1_G4 SPOTS ES_J_1BLFA1 STATIC.
*ENHANCEMENT-POINT J_1BLFA1_G5 SPOTS ES_J_1BLFA1.
*ENHANCEMENT-POINT J_1BLFA1_G6 SPOTS ES_J_1BLFA1 STATIC.
*ENHANCEMENT-POINT J_1BLFA1_G7 SPOTS ES_J_1BLFA1.

*----------------------------------------------------------------------*
*------------------------Report J_1BLFA1-------------------------------*
*----------------------------------------------------------------------*
*-------Creation of the legal file 'Arquivo Magnético'-----------------*
*---------------according to the requirements--------------------------*
*-----------------of Convênio ICMS 69/02 and 142/02 -------------------*
*---------------former Convênio ICMS 31/99 (see J_1BLFA1_OLD)----------*
*---------------former Convênio ICMS 75/96 ----------------------------*
*----------------------------------------------------------------------*

* declaration of the nota fiscal database tables
tables:
        j_1bnfdoc,                     " nota fiscal header,
        j_1bindoc,            " nota fiscal header - add. segment
       *j_1bnfdoc,                     " nota fiscal header,
        j_1bnflin,                     " nota fiscal line items,
        j_1binlin,            " nota fiscal line items - add. segment
        j_1bnfstx,                     " nota fiscal tax per item,
       *j_1bnfstx,                     " nota fiscal tax per item,
        j_1binnad,            " Interface: partner name and address
        j_1blfa1_rec55.       " Table for generating record type 55

* declaration of data used for output on screen (testrun)
tables: sscrfields.

*----------------------------------------------------------------------*

* definition of constants for taxgroups/taxtypes
constants: begin of const_taxgrp,
           icms(4)     type c value 'ICMS',
           ipi(4)      type c value 'IPI ',
           subtrib(4)  type c value 'ICST',
           iczonaf(4)  type c value 'ICZF',
           icmf(4)     type c value 'ICMF',
           icfrei(4)   type c value 'ICFR',
           icstfr(4)   type c value 'ICFS',
           iczg(4)     type c value 'ICZG',                " note 608129
           end of const_taxgrp.

*----------------------------------------------------------------------*

* definition of constants for output
constants: begin of const_out,
           noregio(2)  type c value 'EX',
           nostains(6) type c value 'ISENTO',
           end of const_out.

* internal table which will be filled with all records
data it_record(126) occurs 0 with header line.

*----------------------------------------------------------------------*

* parameters for destination/origin state(s)
selection-screen begin of block b2 with frame title text-102.
select-options: regions for j_1binnad-regio,
                cfops   for j_1bnflin-cfop.
parameters:     assets  as checkbox default space.     " (note 458935)
selection-screen end of block b2.

*----------------------------------------------------------------------*

* parameters for output on screen (testrun) or on file
selection-screen begin of block b1 with frame title text-101.
parameters: testrun  radiobutton group outp default 'X',
            applserv radiobutton group outp,
            presserv radiobutton group outp.
parameters: file_a like sapb-sappfad,
            file_p like sapb-sappfad.               " note 608129
selection-screen end of block b1.

*DATA file_p_help LIKE rlgrap-filename.              " note 608129
data file_p_help type string.                        " note 716358
*----------------------------------------------------------------------*

* parameters for administrative data (Record 10 and 11)
selection-screen begin of block b3 with frame title text-103.
parameters: contact(28) type c.
selection-screen begin of block b4 with frame title text-104.
parameters: intst  radiobutton group typ1,
*           inttot radiobutton group typ1,
            allop  radiobutton group typ1 default 'X',
            inter  radiobutton group typ1.
selection-screen end of block b4.
selection-screen begin of block b5 with frame title text-105.
parameters: normal  radiobutton group typ2 default 'X',
            totadj  radiobutton group typ2.
selection-screen end of block b5.
* record type 10: code of identification of Convenio
*PARAMETER: ident DEFAULT '2'.               " note 720863
parameter: ident default '3'.                " note 720863
selection-screen end of block b3.

*----------------------------------------------------------------------*

* parameters for NF selection (Record 53)
selection-screen begin of block b6 with frame title text-106.
parameters: nfdirall radiobutton group typ3 default 'X',
            nfdirin  radiobutton group typ3,
            nfdirout radiobutton group typ3.
selection-screen end of block b6.

*----------------------------------------------------------------------*

* parameters for NF selection (Record 56)
selection-screen begin of block b7 with frame title text-107.
parameters: rec_56 as checkbox default space.
select-options: matnr for j_1bnflin-matnr.
selection-screen end of block b7.

*----------------------------------------------------------------------*

* parameters for NF selection (Record 74)              " BOI note 637576
selection-screen begin of block b8 with frame title text-108.
parameters: incl74 radiobutton group typ4,
            only74 radiobutton group typ4,
            wout74 radiobutton group typ4 default 'X'.
selection-screen end of block b8.                      " EOI note 637576

*----------------------------------------------------------------------*

* parameters for NF selection (Record 86)              " BOI note 862655
selection-screen begin of block b9 with frame title text-109.
select-options: cfop86 for j_1bnflin-cfop default '1501AA' to '1503ZZ'.
selection-screen end of block b9.                      " EOI note 862655

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*

* declaration of structures coming from function modules
data: address     like addr1_val,
      parnad      like j_1binnad,
      cgc_number  like j_1bwfield-cgc_number,
      branch_data like j_1bbranch,
      t_mesg      like mesg occurs 0 with header line.

*----------------------------------------------------------------------*

* internal table it_records
data: begin of it_records occurs 1000,
      tipo(2)       type n,            "unique type number for record
      pstdat        like j_1bnfdoc-pstdat, "posting date
      series        like j_1bnfdoc-series, "series of the fiscal doc.
      subser        like j_1bnfdoc-subser, "subseries of the fiscal doc.
      nfnum(6)      type c,  "number of fiscal document
      docnum        like j_1bnfdoc-docnum, "internal document number
      cfop_noext(4) type c,   " without ext. ATTENTION: in law numeric!
      rate          like j_1bnfstx-rate,   "taxrate
      matnr         like j_1bnflin-matnr, "material number
*     itmnum        like J_1bnflin-itmnum,              " note 851428
      cgc       like j_1binnad-cgc,    "cgc-number
      stains    like j_1binnad-stains, "regional tax code
      regio     like j_1binnad-regio,  "region
      model     like j_1bnfdoc-model,  "model of fiscal document
      cancel    like j_1bnfdoc-cancel,                " note 608129
      situation like j_1bnfdoc-cancel,"indicator for cancelled documents
      incot     like j_1bnfdoc-inco1,  "freight modalities
      nfnet     like j_1binlin-nfnet,                      " note 762594
      nftot     like j_1binlin-nftot, "total value per item, incl. ICMS/
                                       "ISS, IPI, freight, insurance,
                                       "other expenses
      nfpri     like j_1binlin-nfpri,  "Net price including taxes
      nffre     like j_1binlin-nffre,  "Freight value including taxes
      nfins     like j_1binlin-nfins,  "Insurance value including taxes
      nfoth     like j_1binlin-nfoth,  "Expenses including taxes
      nfdis     like j_1bindoc-nfdis,  "Discount value including taxes
      matuse    like j_1bnflin-matuse, "Material usage
      st_code(3) type c,               "Material usage(1) & sit.trib.(2)
      issuer    type c,  "'P' for self issued, 'T' for 3rd party issued
      issuer2   type n,  "'1' for self issued, '2' for 3rd party issued
      taxsi2    like j_1bnflin-taxsi2, "taxcode for IPI
      nbm       like j_1bnflin-nbm,    "NBM code
      nfunt(3)  type c,                "Unit of measure in NF
      nfqty     like j_1bnflin-menge,  "Quantity
      base      like j_1bnfstx-base,   "calculation base
      stbase    like j_1bnfstx-base,   "calculation base for subst.trib.
      taxval    like j_1bnfstx-taxval, "tax amount
      excbas    like j_1bnfstx-excbas, "excluded base amount
      othbas    like j_1bnfstx-othbas, "other base amount
      docref    like j_1bnfdoc-docref, "reference document
*      itmnum    LIKE j_1bnflin-itmnum, "NF itemnumber     " note 838208
      itmnum    like j_1bnflin-itmnum, "NF itemnumber      " note 851428
      cgc_dest  like j_1binnad-cgc,    "cgc of partner (destination)
      oper_type type n,
      cgc_conc  type j_1bcgc,
      chassi(17) type c,
      parid     type j_1bnfdoc-parid,         " note 637576
      docdat    type j_1bnfdoc-docdat,        " note 637576
end of it_records.

data: wa_records      like it_records,
      it_records54    like it_records occurs 1000,     " BOI note 608129
      it_records_99   like it_records occurs 1000.     " note 851428
data: begin of rec54sum,
        freight      like j_1binlin-nffre,
        insurance    like j_1binlin-nffre,
        expenses     like j_1binlin-nfoth,
      end of rec54sum.                                 " EOI note 608129
*----------------------------------------------------------------------*

* Definition of the output records; structures j_1blfam310/j_1blfa211
* are defined in DDIC according to the legal requirements; other files
* are defined without corresponding DDIC structures
data: rectipo10 like j_1blfa310,
      rectipo11 like j_1blfa211.

* definition of record 50 according requirements of Convenio 69/02
data: begin of rectipo50 occurs 0,
      tipo           type j_1blftipo,
      cgc            type j_1bcgc,
      stains         type j_1blfstai,
      pstdat         type j_1bpstdat,
      regio          type j_1blfreg,
      model          type j_1bmodel,
      series         type j_1blfser3,
      nfnum(6)       type c,
*      cfop_noext(4)  TYPE c,                              " note 838208
      cfop_noext(4)  type n,                               " note 838208
      issuer         type c,
      linnftot       type j_1blftx13,
      stxbase        type j_1blftx13,
      stxtaxval      type j_1blftx13,
      stxexcbase     type j_1blftx13,
      stxothbase     type j_1blftx13,
      rate           type j_1blfrate,
      situation      type j_1bcancel,
end of rectipo50.

* definition of record 51 according requirements of Convenio 69/02
data: begin of rectipo51 occurs 0,
      tipo           type j_1blftipo,
      cgc            type j_1bcgc,
      stains         type j_1blfstai,
      pstdat         type j_1bpstdat,
      regio          type j_1blfreg,
      series(3)      type c,
      nfnum(6)       type c,
*      cfop_noext(4)  TYPE c,                              " note 838208
      cfop_noext(4)  type n,                               " note 838208
      linnftot       type j_1blftx13,
      stxtaxval      type j_1blftx13,
      stxexcbase     type j_1blftx13,
      stxothbase     type j_1blftx13,
      filler(20)     type c,
      situation      type j_1bcancel,
end of rectipo51.

* definition of record 53 according requirements of Convenio 69/02
data: begin of rectipo53 occurs 0,
      tipo           type j_1blftipo,
      cgc            type j_1bcgc,
      stains         type j_1blfstai,
      pstdat         type j_1bpstdat,
      regio          type j_1blfreg,
      model          type j_1bmodel,
      series(3)      type c,
      nfnum          type j_1bnfnumb,
*      cfop_noext(4)  TYPE c,                              " note 838208
      cfop_noext(4)  type n,                               " note 838208
      issuer         type c,
      stxbase        type j_1blftx13,
      stxtaxval      type j_1blftx13,
      expenses       type j_1blftx13,
      situation      type j_1bcancel,
      st_type        type c,                           " note 720863
*      filler(30)     TYPE c,                          " note 720863
      filler(29)     type c,                           " note 720863
end of rectipo53.

* definition of record 54 according requirements of Convenio 69/02
data: begin of rectipo54 occurs 0,
      tipo           type j_1blftipo,
      cgc            type j_1bcgc,
      model          type j_1bmodel,
      series(3)      type c,
      nfnum(6)       type c,
*      cfop_noext(4)  TYPE c,                              " note 838208
      cfop_noext(4)  type n,                               " note 838208
      st_code(3)     type c,                   " note 720863 note 838208
*      st_code(3)     TYPE n,                  " note 720863 note 838208
      itmnum         type j_1blfinu3,
      matnr(14)      type c,
      nfqty(11)      type n,
*      linnftot       TYPE j_1blftx12,     " note 762594
      linnfnet       type j_1blftx12,      " note 762594
      discount       type j_1blftx12,
      icmsbase       type j_1blftx12,
      icstbase       type j_1blftx12,
      stxtaxval      type j_1blftx12,
      rate           type j_1blfrate,
      freight        type j_1blftx12,
      insurance      type j_1blftx12,
      expenses       type j_1blftx12,
end of rectipo54.

* definition of record 55 according requirements of Convenio 69/02
data: begin of rectipo55 occurs 0,
      tipo           type j_1blftipo,
      cgc            type j_1bcgc,
      deststains     type j_1blfstai,
      docdat         type j_1bgnredat,
      regio          type j_1blfreg,
      destregio(2)   type c,  " Attention: R/3 3 digits, reported 2 dig.
      bank           type j_1bgnrebank,
      agency         type j_1bgnreagency,
      gnrenum        type j_1bgnrenum,
      value          type j_1blftx13,
      duedat         type j_1bduedat,
      refmon         type j_1brefmon,
      refyear        type j_1brefyear,
      convnum        type j_1blfconvnum,
end of rectipo55.

* definition of record 56 according requirements of Convenio 69/02
data: begin of rectipo56 occurs 0,
      tipo           type j_1blftipo,
      cgc            type j_1bcgc,
      model          type j_1bmodel,
      series(3)      type c,
      nfnum(6)       type c,
*      cfop_noext(4)  TYPE c,                 " note 720863, note 838208
      cfop_noext(4)  type n,                               " note 838208
      st_code(3)     type c,                   " note 720863 note 838208
*      st_code(3)     TYPE n,                  " note 720863 note 838208
      itmnum         type j_1blfinu3,
      matnr(14)      type c,
      oper_type      type n,
      cgc_conc       type j_1bcgc,
      rate           type j_1blfrate,
      chassi(17)     type c,
end of rectipo56.

* definition of record 70 according requirements of Convenio 69/02
data: begin of rectipo70 occurs 0,
      tipo           type j_1blftipo,
      cgc            type j_1bcgc,
      stains         type j_1blfstai,
      pstdat         type j_1bpstdat,
      regio          type j_1blfreg,
      model          type j_1bmodel,
      series         type j_1blfser1,
      subser         type j_1blfsubs,
      nfnum(6)       type c,
*      cfop_noext(4)  TYPE c,                              " note 838208
      cfop_noext(4)  type n,                               " note 838208
      linnftot       type j_1blftx13,
      stxbase        type j_1blftx14,
      stxtaxval      type j_1blftx14,
      stxexcbase     type j_1blftx14,
      stxothbase     type j_1blftx14,
      incot          type j_1blffrgt,
      situation      type j_1bcancel,
end of rectipo70.

* definition of record 71 according requirements of Convenio 69/02
data: begin of rectipo71 occurs 0,
      tipo            type j_1blftipo,
      cgc             type j_1bcgc,
      stains          type j_1blfstai,
      pstdat          type j_1bpstdat,
      regio           type j_1blfreg,
      model           type j_1bmodel,
      series          type j_1blfser1,
      subser          type j_1blfsubs,
      nfnum(6)        type c,
      regio2          type j_1blfreg,
      cgc2            type j_1bcgc,
      stains2         type j_1blfstai,
      issuedat        type j_1bpstdat,
      model_relnf     type j_1bmodel,
      series_relnf(3) type c,
      nfnum_relnf     type j_1bnfnumb,
      linnftot        type j_1blftx14,
      filler(12)      type c,
end of rectipo71.

* definition of record 74 according requirements of Convenio 69/02
data: begin of rectipo74 occurs 0,
      tipo             type j_1blftipo,
      invdat           type j_1bpstdat,
      matnr(14)        type c,
      mat_quantity(13) type n,
      total_cost(13)   type n,
      location         type c,
      cgc              type j_1bcgc,
      stains           type j_1blfstai,
      regio            type j_1blfreg,
      filler(45)       type c,
end of rectipo74.

* definition of record 75 according requirements of Convenio 31/99;
* unchanged for Convenio 69/02
data: begin of rectipo75 occurs 0,
      tipo(2)       type n,
      inidate(8)    type n,
      finaldate(8)  type n,
      matnr(14)     type c,
      nbm(8)        type c,
      decript(53)   type c,
      meins(6)      type c,
*      st_code(3)    TYPE c,            " note 608129      note 720863
*      matorg        LIKE j_1bnflin-matorg,           " note 608129
*      taxsit(2)     TYPE c,                          " note 608129
*      ipirate(4)    TYPE n,                             " note 720863
      ipirate(5)    type n,                              " note 720863
      icmsrate(4)   type n,
*      icmsbasred(4) TYPE n,                             " note 720863
      icmsbasred(5) type n,                              " note 720863
      subtrib(13)   type n,                              " note 720863
      sort_pur_sal  type c, " sort. param., no output, only intern. used
      matnr_long    like j_1bnflin-matnr, " no output, only intern. used
      direct        like j_1bnfdoc-direct," no output, only intern. used
      entrad        like j_1bnfdoc-entrad," no output, only intern. used
      cancel        type c,       " no output                note 608129
      nbm_orig(16)  type c,                                " note 608129
      counter(4)    type n,                                " note 608129
end of rectipo75.

* definition of record 76 according requirements of Convenio 69/02
data: begin of rectipo76 occurs 0,
      tipo           type j_1blftipo,
      cgc            type j_1bcgc,
      stains         type j_1blfstai,
      model          type j_1bmodel,
      series(2)      type c,           " 2 digits !
      subser         type j_1blfsubs,
      nfnum(10)      type n,           " 10 digits!
*      cfop_noext(4)  TYPE c,                              " note 838208
      cfop_noext(4)  type n,                               " note 838208
      issuer2        type n,           " '1'/'2' instead of 'P'/'T'
      pstdat         type j_1bpstdat,
      regio          type j_1blfreg,
      linnftot       type j_1blftx13,
      stxbase        type j_1blftx13,
      stxtaxval      type j_1blftx12,
      stxexcbase     type j_1blftx12,
      stxothbase     type j_1blftx12,
      rate(2)        type n,           "  only 2 digits!
      situation      type j_1bcancel,
end of rectipo76.

* definition of record 77 according requirements of Convenio 69/02
data: begin of rectipo77 occurs 0,
      tipo           type j_1blftipo,
      cgc            type j_1bcgc,
      model          type j_1bmodel,
      series(2)      type c,           " 2 digits !
      subser         type j_1blfsubs,
      nfnum(10)      type n,           " 10 digits!
*      cfop_noext(4)  TYPE c,                              " note 838208
      cfop_noext(4)  type n,                               " note 838208
      issuer2        type n,           " '1'/'2' instead of 'P'/'T'
      itmnum         type j_1blfinu3,
      matnr(11)      type c,
      nfqty(13)      type n,
      linnftot       type j_1blftx12,
      discount       type j_1blftx12,
      icmsbase       type j_1blftx12,
      rate(2)        type n,           "  only 2 digits!
      cgc_dest       type j_1bcgc,
      num_termin(10) type n,
end of rectipo77.

* Begin of note 862655
* Def. of rec. 85 according requirements of Convenio 20/04 and 15/05
data: begin of rectipo85 occurs 0,
      tipo              type j_1blftipo,
      de_number(11)     type c,
      de_date           type d,
      export_reason     type c,
      export_regist(12) type c,
      regist_date       type d,
      conheci_numb(16)  type c,
      conheci_date      type j_1bpstdat,
      conheci_type(2)   type n,
*      country          TYPE T005-LAND1,    " note 927319
      country           type char4,           " note 927319
      filler1(8)        type c,
      averbacao_date    type d,
      nfnum             type j_1bnfnumb,
      pstdat            type j_1bpstdat,
      model             type j_1bmodel,
      series(3)         type n,
      filler2(19)       type c,
end of rectipo85.

* Def. of rec. 86 according requirements of Convenio 20/04 and 15/05
data: begin of rectipo86 occurs 0,
      tipo              type j_1blftipo,
      de_number(12)     type n,
      de_date           type d,
      cgc               type j_1bcgc,
      stains            type j_1blfstai,
      regio             type j_1blfreg,
      nfnum             type j_1bnfnumb,
      pstdat            type j_1bpstdat,
      model             type j_1bmodel,
      series            type j_1blfser3,
      matnr(14)         type c,
      nfqty(11)         type n,
      nfpri(12)         type n,
      nfnet(12)         type n,
      relation          type c,
      filler(5)         type c,
end of rectipo86.
* End of note 862655

* definition of record 90 according requirements of Convenio 31/99
* unchanged for Convenio 69/02
data: begin of rectipo90,
      tipo(2)       type n,
      cgc           type j_1bcgc,
      state_insc    type j_1blfstai,
      numtip50      like j_1blfa290-numtip50 value '99',
      count50       like j_1blfa290-count50 value '99999999',
      numtip51      like j_1blfa290-numtip51 value '99',
      count51       like j_1blfa290-count51 value '99999999',
      numtip53      like j_1blfa290-numtip53 value '99',
      count53       like j_1blfa290-count53 value '99999999',
      numtip54      like j_1blfa290-numtip54 value '99',
      count54       like j_1blfa290-count54 value '99999999',
      numtip55      like j_1blfa290-numtip54 value '99',
      count55       like j_1blfa290-count54 value '99999999',
      numtip56      like j_1blfa290-numtip54 value '99',
      count56       like j_1blfa290-count54 value '99999999',
      numtip70      like j_1blfa290-numtip70 value '99',
      count70       like j_1blfa290-count70 value '99999999',
      numtip71      like j_1blfa290-numtip70 value '99',
      count71       like j_1blfa290-count70 value '99999999',
      numtip74(2)   type n value '99',
      count74(8)    type n value '99999999',
      numtip75(2)   type n value '99',
      count75(8)    type n value '99999999',
      numtip76      like j_1blfa290-numtip54 value '99',
      count76       like j_1blfa290-count54 value '99999999',
      numtip77      like j_1blfa290-numtip54 value '99',
      count77       like j_1blfa290-count54 value '99999999',
      numtip85      like j_1blfa290-numtip54 value '99',    " 862655
      count85       like j_1blfa290-count54 value '99999999'," 862655
      numtip86      like j_1blfa290-numtip54 value '99',    " 862655
      count86       like j_1blfa290-count54 value '99999999'," 862655
*      filler(15)    TYPE c,                               " note 608129
*      count9001     LIKE j_1blfa290-count9001,            " note 608129
      numtip99      like j_1blfa290-numtip90,              " note 608129
      countall      like j_1blfa290-countall,              " note 608129
end of rectipo90.

*----------------------------------------------------------------------*

* declaration of internal table for tax types ( = j_1baj)
data: intj_1baj like j_1baj occurs 0 with header line.

* declaration of internal table for nota fiscal types ( = j_1baa)
data: intj_1baa like j_1baa occurs 0 with header line,
      lines_intj_1baa type i.
*----------------------------------------------------------------------*

* declaration of internally used table to determine the type of record
* from a given modelo number of a documento fiscal and from the type of
* tax/taxgroup (ICMS, IPI, Substitucao Tributaria)
data: begin of tipo_table occurs 14,
      model    like j_1bnfdoc-model,
      taxgroup like j_1baj-taxgrp,
      tipo     like j_1blfamcm-tipo,
end of tipo_table.

*----------------------------------------------------------------------*

* declaration of internally used help-fields
data: count50  type i,                 "counter records type 50
      count51  type i,                 "counter records type 51
      count53  type i,                 "counter records type 53
      count54  type i,                 "counter records type 54
      count55  type i,                 "counter records type 55
      count56  type i,                 "counter records type 56
      count70  type i,                 "counter records type 70
      count71  type i,                 "counter records type 71
      count74  type i,                 "counter records type 74
      count75  type i,                 "counter records type 75
      count76  type i,                 "counter records type 76
      count77  type i,                 "counter records type 77
      count85  type i,                 "counter records type 85 " 862655
      count86  type i,                 "counter records type 86 " 862655
      countall type i,                 "counter for all record types
      recordnumb   type i,
      tipo     like j_1blfamcm-tipo.
data: orig_pstdat     like j_1bnfdoc-pstdat,          " corr note 720863
      matdoc_budat    like j_1bnfdoc-pstdat,          " corr note 720863
      flag_first_item type c.                         " corr note 720863

*----------------------------------------------------------------------*

* declaration of internally used help-table with fields that have to be
* condensed (summed up) by taxrate and/or CFOP number
data: begin of taxsums,
      linnfnet   like j_1binlin-nfnet,                     " note 762594
      linnftot   like j_1binlin-nftot, "totel value per item
      expenses   like j_1binlin-nfoth, "expenses per item
      stxbase    like j_1bnfstx-base,  "calculation base
      stxstbase  like j_1bnfstx-base,  "calculation base subst. trib.
      stxtaxval  like j_1bnfstx-taxval,"tax amount
      stxexcbase like j_1bnfstx-excbas,"excluded base amount
      stxothbase like j_1bnfstx-othbas,"other base amount
end of taxsums.

*----------------------------------------------------------------------*

* other help fields
*DATA: part2(120)             TYPE c,                      " note 608129
*DATA: part2(130)             TYPE c,   " note 608129
data: part2(150)             type c,   " note 862655
      lastrectipo54(3)       type n,   " item number for record 54
      lastrectipo56(3)       type n,   " note 608129
      lastrectipo77(3)       type n,   " note 608129
      last_taxrate           like j_1bnfstx-rate,
      icms_first_tax_item    type c,
      help_taxsit(2)         type c,
      tmp_nbm                like j_1bnflin-nbm,
*      matnr_short            LIKE j_1bnflin-matnr,        " note 660429
      matnr_short(14)        type c,   " note 660429
      message_processed      type c.                       " note 608129
data: ld_dummy type string,                                " note 716358
      file_p_2 type string.                                " note 716358

*----------------------------------------------------------------------*

* declaration of fields required to handle versioned CFOPs/NF item types
types: tab_nfitmrule type standard table of
       j_1bnfitmrule with default key.
data:  gt_nfitmrule type tab_nfitmrule,
       gs_nfitmrule type j_1bnfitmrule,
       cfop_length  type j_1bcfop_len.

*----------------------------------------------------------------------*
* declaration of required definitions for creation of record 74
*----------------------------------------------------------------------*
constants: in_68(1) type c value '3'.
tables:  mbew,                         "Material Valuation
         mbewh.                        "MBEW History

* declaration of variables
data:  lines           type i,         " no. of plants per cgc-comp.
       rep_date_high        type d.

* declaration of internal tables
data: is_t134m like t134m,
      it_msku like table of msku with header line,
      it_mslb like table of mslb with header line.

* declaration of structures
data: begin of summe,   " needed to sum up over all plants
        tot_quant    like mbew-vmkum,  " tot. quant. (mbew)
        tot_cost(16) type p decimals 2," cost from mbew
        deb_quant    like mbew-vmkum,  " deb. consig. (msku)
        cre_quant    like mbew-vmkum,  " cred. consig. (mslb)
        own_quant    like mbew-vmkum,  " own stock w/o consignments
      end of summe.
data: begin of material occurs 0,
        matnr like mara-matnr,
        mtart like mara-mtart,
        meins like mara-meins,
      end of material.
data: begin of plants occurs 0,
        sign(1),
        option(2),
        low  like t001w-werks,
        high like t001w-werks,
      end of plants.
data: begin of it_consig_summe occurs 0,
        parid        like j_1bnfdoc-parid,
        deb_quant    like mbew-vmkum,  " debitor consigment  (msku)
        cre_quant    like mbew-vmkum,  " creditor consignment (mslb)
      end of it_consig_summe,
      wa_consig_summe   like it_consig_summe,
      ls_kna1           like kna1,
      ls_lfa1           like lfa1.
data: lcl_tabix like sy-tabix,
      last_record like it_records.
data: periv like t001-periv,
      buper like t009b-poper,
      gjahr like t009b-bdatj.

ranges: ipi_types for j_1baj-taxtyp.                       " note 699257
data: countrectipo54(3) type n,                            " note 863634
       previous_nfnum like rectipo54-nfnum.
* definition of BADI for record 56
class cl_ex_badi_j_1blfa1 definition load.
data if_ex_badi_j_1blfa1 type ref to if_ex_badi_j_1blfa1.

* BADI definition for legal books                             nt. 617905
class cl_ex_badi_j_1blegalreport definition load.           " nt. 617905
data if_ex_badi_j_1blegalreport type ref to if_ex_badi_j_1blegalreport.

data: lf_export_processing  type c.                        " note 862655

* begin of note 959823
data lv_ind_ignore85 type char1.
data lv_ind_ignore86 type char1.
data lt_rec85 type j_1blfa1_rec85tt.
data ls_rec85 type j_1blfa1_rec85.
data lt_rec86 type j_1blfa1_rec86tt.
data ls_rec86 type j_1blfa1_rec86.
* end of note 959823

*----------------------------------------------------------------------*
*--------------------at selection-screen-------------------------------*
*----------------------------------------------------------------------*

at selection-screen.

* determine company/branch data
  call function 'J_1BREAD_BRANCH_DATA'
    exporting
      branch      = j5_brnch     "branch code
      bukrs       = j5_bukrs     "company code
    importing
      address1    = address
      branch_data = branch_data
      cgc_number  = cgc_number
    exceptions
      others      = 04.

* check if company/branch data is maintained
  if sy-subrc ne 0.
    message w453 with j5_bukrs j5_brnch.
  endif.

*--------------------------------------------------- BOI note 617905 --*
* build an instance of the object -------------------------------------*
*----------------------------------------------------------------------*
  call method cl_exithandler=>get_instance
    changing
      instance = if_ex_badi_j_1blegalreport.        " EOI note 617905

* Determine reporting period
  if j5_pdate-high is initial.
    rep_date_high = j5_pdate-low.
  else.
    rep_date_high = j5_pdate-high.
  endif.

* fill table intj_1baa and give a warning, when not for all NF types
* the NF model is maintained
  select * from j_1baa into table intj_1baa where model <> '00'.
  describe table intj_1baa lines lines_intj_1baa.
  select count( * ) from j_1baa.
*  IF sy-dbcnt > lines_intj_1baa.                          " note 608129
  if sy-dbcnt > lines_intj_1baa and message_processed is initial."608129
    if only74 is initial.  " no message, when no NF selection, 637576
      message w494.
      message_processed = 'X'.                             " note 608129
    endif.                                                 " note 637576
  endif.

* check if input on selection screen was complete
  if ( applserv = 'X' and file_a is initial ) or
                               ( presserv = 'X' and file_p is initial ).
    message e451.
  endif.

* If rec_56 is set but no material informed, give the warning "Please
* inform a material code", otherwise no materials are considered and no
* records 56 are created
  if rec_56 = 'X' and matnr is initial.                    " note 608129
    message w663.                                          " note 608129
  endif.                                                   " note 608129

  if applserv = 'X'.                   "download on application server
* check, if file on application server already exists
    open dataset file_a for input in text mode
    encoding non-unicode ignoring conversion errors.
    if sy-subrc eq 0.
      message w479.
      close dataset file_a.
    else.
*   check, if filename is valid
      open dataset file_a for output in text mode
      encoding non-unicode ignoring conversion errors.
      if sy-subrc is initial.
        close dataset file_a.
        delete dataset file_a.         "only opened to test filename
      else.
        message e452 with file_a.
      endif.
    endif.
  elseif presserv = 'X'.     " download to pres. server; BOI note 716358
* Dialog, if file already exists; data are tranfered later via
* FuBa ARCHIVFILE_SERVER_TO_CLIENT
    file_p_help = file_p.          " required because of the data format
    call method cl_gui_frontend_services=>file_save_dialog
      exporting
        default_file_name = file_p_help
        initial_directory = file_p_help
      changing
        filename          = file_p_2
        path              = ld_dummy
        fullpath          = file_p_help.

    if file_p_help is initial.
      exit.
    endif.
  endif.                                               " EOI note 716358

* for presentation server: not possible as background job
*                          dialog at start-of-selection
  if presserv = 'X' and
     sscrfields-ucomm = 'SJOB'.
    message e480.   " download on pres. server not poss. as batch job
  endif.

* determination of the CFOP-version
  call function 'J_1B_CFOP_GET_VERSION'
    exporting
      land1             = address-country
      region            = address-region
      date              = j5_pdate-low
    importing
      cfoplength        = cfop_length
    exceptions
      date_missing      = 1
      version_not_found = 2
      others            = 3.

  if sy-subrc <> 0.
    message e035.  " no valid CFOP could be determined
  endif.

* check if period is valid for reading MBEWH values. Perform this check
* only if record/s 74 have to be created. Furthermore, take care if a
* special fiscal year variant is used.
  if wout74 is initial.                              " BOI note 637576

    select single periv from t001 into periv where bukrs = j5_bukrs.
    call function 'DATE_TO_PERIOD_CONVERT'
      exporting
        i_date         = rep_date_high
        i_periv        = periv
      importing
        e_buper        = buper
        e_gjahr        = gjahr
      exceptions
        input_false    = 1
        t009_notfound  = 2
        t009b_notfound = 3
        others         = 4.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
             with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    call function 'J_1B_CHECK_NEW_PERIOD_CLOSING'
      exporting
        i_bukrs           = j5_bukrs
        i_lfgja           = gjahr
        i_lfmon           = buper+1(2)
        i_caller          = in_68
      exceptions
        period_not_closed = 1
        not_possible      = 2
        no_marv_for_bukrs = 3
        only_mbew         = 4
        future_period     = 5
        others            = 6.
    case sy-subrc.
      when 1.
        message w474 with  rep_date_high+4(2)  rep_date_high(4).
      when 2.                 "No data available for period &1 &2
        message e484 with  rep_date_high+4(2)  rep_date_high(4).
      when 3.
        message e476 with j5_bukrs.
      when 4.
*          ONLY_MBEW = 'X'.
      when 5.
        message e477 with  rep_date_high+4(2) rep_date_high(4).
      when 6.
* ??????????????????
    endcase.

  endif.                                               " EOI note 637576

*----------------------------------------------------------------------*
*---------------------start-of-selection-------------------------------*
*----------------------------------------------------------------------*

start-of-selection.

  if presserv = 'X' and not sy-batch is initial.
    message e480.
  endif.

*----------------------------------------------------------------------*
*-----Delete messages collected up to now, collect future messages-----*
*----------------------------------------------------------------------*
  call function 'MESSAGES_INITIALIZE'.

*----------------------------------------------------------------------*
*-----------------------fill table tipo-table--------------------------*
*----------------------------------------------------------------------*
  perform fill_tipo_table using: '01' 'ICMS' '50',
                                 '01' 'ICMS' '54',
                                 '01' 'IPI'  '51',
                                 '01' 'IPI'  '54',
                                 '01' 'ICST' '53',
                                 '01' 'ICFR' '53',
                                 '01' 'ICFS' '53',
                                 '01' 'ICMS' '56',
                                 '03' 'ICMS' '50',
                                 '03' 'ICMS' '54',
                                 '03' 'IPI'  '51',
                                 '03' 'IPI'  '54',
                                 '03' 'ICST' '53',
                                 '03' 'ICFS' '53',
                                 '03' 'ICFR' '53',
                                 '06' 'ICMS' '50',
                                 '06' 'ICMS' '54',
                                 '06' 'ICST' '53',
                                 '06' 'ICFR' '53',
                                 '06' 'ICFS' '53',
                                 '21' 'ICMS' '50',         " note 783404
                                 '21' 'ICMS' '54',         " note 783404
                                 '21' 'ICMS' '76',
                                 '21' 'ICMS' '77',
                                 '22' 'ICMS' '50',         " note 783404
                                 '22' 'ICMS' '54',         " note 783404
                                 '22' 'ICMS' '76',
                                 '22' 'ICMS' '77',
                                 '22' 'ICST' '53',
                                 '22' 'ICFS' '53',
                                 '22' 'ICFR' '53',
                                 '07' 'ICMS' '70',
                                 '08' 'ICMS' '70',
                                 '09' 'ICMS' '70',
                                 '10' 'ICMS' '70',
                                 '08' 'ICMS' '71',        " note 720863
                                 '09' 'ICMS' '71',
*                                 '10' 'ICMS' '71'.        " note 720863
                                 '10' 'ICMS' '71',         " note 720863
                                 '11' 'ICMS' '71'.         " note 720863


  sort tipo_table.

*----------------------------------------------------------------------*
*-- fill tables intj_1baj and gt_nfitmrule ----------------------------*
*----------------------------------------------------------------------*
  select * from j_1baj into table intj_1baj order by primary key.

  if sy-subrc ne 0.
    message a460.      " An error occured while reading table j_1baj
  endif.

  loop at intj_1baj where taxgrp = const_taxgrp-ipi.   " BOI note 699257
    ipi_types-sign   = 'I'.
    ipi_types-option = 'EQ'.
    ipi_types-low    = intj_1baj-taxtyp.
    append ipi_types.
  endloop.                                             " EOI note 699257

  select * from j_1bnfitmrule into table gt_nfitmrule.

*----------------------------------------------------------------------*
*--fill record tipo 10 with company data ('Mestre do Estabelecimento')-*
*----------------------------------------------------------------------*
  move: '10'                   to rectipo10-tipo,     "record type
        cgc_number             to rectipo10-cgc,      "cgc number
        branch_data-name       to rectipo10-name,     "company name
        address-city1          to rectipo10-city,     "city
        address-region         to rectipo10-regio,    "region
        address-fax_number     to rectipo10-fax,      "fax number
        j5_pdate-low           to rectipo10-inidate,  "initial date
        j5_pdate-high          to rectipo10-finaldate."final date

* delete special characters (., -, /) from regional tax code
  clear rectipo10-stains.
  perform format_stains using branch_data-state_insc
                        changing rectipo10-stains.
  branch_data-state_insc = rectipo10-stains.

* field rectipo10-ident filled with parameter from selection screen
* currently to be filled with '1',
  rectipo10-ident = ident.

* depending on the entries on the selection screen, rectipo10-type2 is
* filled. The law gives four possible values:
  if intst = 'X'.
    rectipo10-type2 = '1'.
  endif.
*if inttot = 'X'.
*  rectipo10-type2 = '02'.
*endif.
  if allop = 'X'.
    rectipo10-type2 = '3'.
  endif.
  if inter = 'X'.
    rectipo10-type2 = '2'.
  endif.

* depending on the entries on the selection screen, rectipo10-type1 is
* filled. the law itself gives five possible values, the report can only
* be used to generate two of these:
  if normal = 'X'.                     "normal file: total information
    rectipo10-type1 = '1'.
  endif.
  if totadj = 'X'.         "adjustment of former file: total information
    rectipo10-type1 = '2'.   " = complete substitution of former file
  endif.

  perform output using rectipo10.

*----------------------------------------------------------------------*
*--fill record tipo 11 with company data ('Dados complementares...')---*
*----------------------------------------------------------------------*
  move: '11'                   to rectipo11-tipo,       "record type
        address-street         to rectipo11-street,     "street
        address-house_num1     to rectipo11-housenumber,"housenumber
        address-floor          to rectipo11-complement(10), "floor
        address-roomnumber     to rectipo11-complement+11(10), "roomno.
        address-city2          to rectipo11-district,   "district
        address-post_code1     to rectipo11-zipcode,    "zipcode
        contact                to rectipo11-contact,    "contact person
        address-tel_number     to rectipo11-telephone.  "telephone no.

  perform output using rectipo11.

*--------------------------------------------------- BOI note 660429 --*
* build an instance of the object -------------------------------------*
*----------------------------------------------------------------------*
  call method cl_exithandler=>get_instance
    changing
      instance = if_ex_badi_j_1blfa1.              " EOI note 660429

*----------------------------------------------------------------------*
* get all plants of the chosen branch (required for creation of rec. 74)
  if wout74 is initial.                   " note 637576
    perform fill_plants.
  endif.                                  " note 637576

*---------------------------------------------------- BOI note 637576 -*
* run the NF selection only if the selection parameter for "only
* creation of records 74 is not set" and fill the material table not via
* NFs but report all materials for the company code/plant
  if not only74 is initial.
    clear j5_brnch.
    perform fill_products.
  endif.                                            " EOI note 637576
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*- create internal table with information of the nota-fiscal database -*
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*----------------------------get j_1bnfdoc-----------------------------*
*----------------------------------------------------------------------*

get j_1bnfdoc.

*----------------------------------------------------------------------*
* build an instance of the object ------------------------------------
*----------------------------------------------------------------------*
*  CALL METHOD cl_exithandler=>get_instance               " note 660429
*    CHANGING                                             " note 660429
*      instance = if_ex_badi_j_1blfa1.                    " note 660429

* clear the cancel flag in case of cancellation of the NF (CANDAT) after
* end of selected reporting period
  if j_1bnfdoc-cancel = 'X'.                           " BOI note 617905
    if j_1bnfdoc-candat gt rep_date_high.
      clear j_1bnfdoc-cancel.
    endif.                                             " EOI note 617905
* enhancement of note 617905: BADI offered for modification of the
* cancel-flag for specific purposes
    call method if_ex_badi_j_1blegalreport->cancel_treatment
      exporting
        is_nfdoc  = j_1bnfdoc
      importing
        cancel_id = j_1bnfdoc-cancel.
  endif.

  clear: it_records.

  clear: lf_export_processing.                             " note 862655

* exclude documents with external document number (NF number) '000000'
  check j_1bnfdoc-nfnum ne '000000'.
* Excluir os documentos que foram criados pela INTERFACE.
  check j_1bnfdoc-CRENAM ne 'INTERFACE'.

* exclude documents that were only created to cancel another document
* indicator: documenttype (j_1bnfdoc-doctyp) = 5
  check j_1bnfdoc-doctyp ne '5'.

* don't report corrections
  check j_1bnfdoc-doctyp ne '3'.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input        = j_1bnfdoc-nfnum
   IMPORTING
     OUTPUT        = j_1bnfdoc-nfnum.

  move-corresponding j_1bnfdoc to it_records.

* determine if the document itself was cancelled
* rec. 50 (fld. 17)/rec. 51 (fld. 14) - Situation / Status of NF:
* NF in same period (month of docdat = pstdat): Normal NF = N; cancelled
* NF = S; NF in other period: Normal NF = E; cancelled NF = X
*  CASE j_1bnfdoc-cancel.                         " BOD corr note 720863
*    WHEN 'X'.
*      MOVE 'X' TO it_records-cancel.          " note 608129
*      IF j_1bnfdoc-pstdat+4(2) = j_1bnfdoc-docdat+4(2).
*        MOVE 'S' TO it_records-situation.
*      ELSE.
*        MOVE 'X' TO it_records-situation.
*      ENDIF.
*    WHEN ' '.
*      IF j_1bnfdoc-pstdat+4(2) = j_1bnfdoc-docdat+4(2).
*        MOVE 'N' TO it_records-situation.
*      ELSE.
*        MOVE 'E' TO it_records-situation.
*      ENDIF.
*  ENDCASE.                                       " EOD corr note 720863
  flag_first_item = 'X'.                              " corr note 720863

* don't report corrections
  check j_1bnfdoc-doctyp ne '3'.

  clear parnad.

* determine if the NF was issued by own branch ('P'), or by others ('T')
  case j_1bnfdoc-direct.
    when '1'.
      if j_1bnfdoc-entrad = 'X'.
        move 'P' to it_records-issuer.
        move '1' to it_records-issuer2.
      else.
        move 'T' to it_records-issuer.
        move '2' to it_records-issuer2.
      endif.
    when '2'.
      move 'P' to it_records-issuer.
      move '1' to it_records-issuer2.
  endcase.

  case j_1bnfdoc-inco1.
    when 'CIF'.
      move '1' to it_records-incot.
    when 'FOB'.
      move '2' to it_records-incot.
    when others.                          " note 862655
      move '0' to it_records-incot.       " note 862655
  endcase.

* check, that J_1BNFDOC-MODEL is filled, if not: Emergency Determination
* of the NF model via NF type
  if j_1bnfdoc-model is initial.
    read table intj_1baa with key nftype = j_1bnfdoc-nftype
                                              into j_1bnfdoc-model.
  endif.

* determine data of partner for currently processed fiscal document
  call function 'J_1B_NF_PARTNER_READ'
    exporting
      partner_type     = j_1bnfdoc-partyp
      partner_id       = j_1bnfdoc-parid
      partner_function = j_1bnfdoc-parvw
      doc_number       = j_1bnfdoc-docnum
    importing
      parnad           = parnad
    exceptions
      others           = 04.

* In record 70 field 5 the state from the partner type RE (bill to
* party) of the nota fiscal must be reported (no the original partner)
  if  j_1bnfdoc-model = '07' and j_1bnfdoc-parvw <> 'RE'.
    perform determine_partner_state.
  endif.

* if only for some selected regions of the partner, the Notas Fiscais
* have to be reported, exclude the other NFs (ex: branch in Parana =>
* only NFs with partners in Parana)
  check parnad-regio in regions.

* if the partner has got no cgc-number (overseas operations or partner
* not obliged to have a cgc-number), cgc-field keeps its initial value,
* i.e. zeroes
* if the partner has got no insc. est. (stains) (overseas operations or
* partner not obliged to have a insc. est.), the literal 'ISENTO' is
* assigned to this field
  if parnad-stains is initial.
    move const_out-nostains to parnad-stains.
  endif.

* if the partner is not brazilian, literal 'EX' is assigned to region
  if parnad-land1 ne address-country.
    move const_out-noregio to parnad-regio.
    clear parnad-cgc.
    clear parnad-cpf.
    move const_out-nostains to parnad-stains.
  endif.

  move-corresponding parnad to it_records.

* in case of natural person, CGC is empty, CPF must be reported
  if parnad-stkzn = 'X'.
    it_records-cgc(3) = '000'."CPF has length 11, => 3 leading zeros
    it_records-cgc+3(11) = parnad-cpf.
  endif.

* delete ., -, / from regional tax code
  if not parnad-stains is initial and
     not parnad-stains eq const_out-nostains.
    clear it_records-stains.
    perform format_stains using parnad-stains
                       changing it_records-stains.
  endif.

*----------------------------------------------------------------------*
*----------------------------get j_1bindoc-----------------------------*
*----------------------------------------------------------------------*

get j_1bindoc.

  if intst = 'X'.
    check j_1bindoc-icstbase > 0.
  endif.

*----------------------------------------------------------------------*
*----------------------------get j_1bnflin-----------------------------*
*----------------------------------------------------------------------*

get j_1bnflin.

  clear: it_records-cfop_noext,
         it_records-taxsi2,
         it_records-nfunt,
         it_records-nfqty.

  icms_first_tax_item = 'X'.  " => allow only one ICMS tax line per item

* do not consider service items
  check j_1bnflin-tmiss = ' '.

* convert encoded CFOP in 6 digit form                 " BOI note 779830
  write j_1bnflin-cfop to j_1bnflin-cfop.
* guarantee that 3 digit CFOP is not reported as '111/', but '111 '
  replace '/' with ' ' into j_1bnflin-cfop.
  condense j_1bnflin-cfop no-gaps.                     " EOI note 779830

* do only consider chosen CFOPs
  check j_1bnflin-cfop in cfops.

  move-corresponding j_1bnflin to it_records.
  move j_1bnflin-matnr to it_records-matnr.
  move j_1bnflin-menge to it_records-nfqty.
  write j_1bnflin-meins to it_records-nfunt.

* 'Situação Tributária' code: filled in as per material origin and
* 'Situação Tributária' informed in the Nota Fiscal
* write statement necessary to convert the field (convertion routine!)
  write j_1bnflin-taxsit    to help_taxsit.            " note 720863
* fill 2nd&3rd didigt of 'Situação Tributária' with zeros
*  help_taxsit = '00'.                                 " note 720863
  concatenate j_1bnflin-matorg help_taxsit into it_records-st_code.

  if flag_first_item = 'X'.                       " BOI corr note 720863
    perform filling_situation.
    clear flag_first_item.
  endif.                                          " EOI corr note 720863

*----------------------------------------------------------------------*
*----------------------------get j_1binlin-----------------------------*
*----------------------------------------------------------------------*

get j_1binlin.

  clear: it_records-nftot.

  move j_1bnflin-cfop(cfop_length) to it_records-cfop_noext.

* fill internal table with NF line item information
  move-corresponding j_1binlin to it_records.

  write j_1bnflin-meins to it_records-nfunt.          " corr note 720863

* Attention: DOCREF exists for J_1BNFDOC and J_1BNFLIN!
  move j_1bnfdoc-docref to it_records-docref.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
  exporting
    input        = j_1bnfdoc-nfnum
  importing
    output       = it_records-nfnum.


* fill valor contabil with zeroes in case of second NF in third party
* process (third party GR item supplier to customer) is reported without
* any values in Modelo 1 and 2. - Will therefore be reported as 0 here
  read table gt_nfitmrule into gs_nfitmrule
        with key itmtyp = j_1bnflin-itmtyp.
  if gs_nfitmrule-trdparty = 'X'.
    clear it_records-nftot.
  endif.

  if j_1bnflin-cfop(1) = '7' and                     " Begin note 862655
            lf_export_processing is initial.
    move '85' to it_records-tipo.
    lf_export_processing = 'X'.
    append it_records.
  endif.

  if j_1bnflin-cfop in cfop86.
    move '86' to it_records-tipo.
    append it_records.
  endif.                                               " End note 862655


*----------------------------------------------------------------------*
*----------------------------get j_1bnfstx-----------------------------*
*----------------------------------------------------------------------*

get j_1bnfstx.

*--> Do not consider incoming tax items for                " note 608129
*--> Zona Franca offsetting                                " note 608129
  check j_1bnfstx-taxtyp <> const_taxgrp-iczg.             " note 608129

  clear: it_records-rate,
         it_records-base,
         it_records-stbase,
         it_records-taxval,
         it_records-excbas,
         it_records-othbas.

  clear intj_1baj.

  read table intj_1baj with key taxtyp = j_1bnfstx-taxtyp
                       binary search.
  if sy-subrc ne 0.
* write message to errorlog
    call function 'MESSAGE_STORE'
      exporting
        arbgb = '8B'
        msgty = 'E'
        msgv1 = j_1bnflin-reftyp
        msgv2 = j_1bnflin-refkey
        msgv3 = j_1bnfdoc-nfnum
        msgv4 = j_1bnfstx-taxtyp
        txtnr = '456'.
    reject.
  endif.

  check intj_1baj-taxgrp = const_taxgrp-icms
     or intj_1baj-taxgrp = const_taxgrp-ipi
     or intj_1baj-taxgrp = const_taxgrp-subtrib
     or intj_1baj-taxgrp = const_taxgrp-icfrei
     or intj_1baj-taxgrp = const_taxgrp-icstfr.

  check j_1bnfstx-taxtyp ne const_taxgrp-icmf.

  if intj_1baj-taxgrp = const_taxgrp-icms and
       ( j_1bnfstx-rate <> last_taxrate and
                     not j_1bnfstx-rate is initial ).
    icms_first_tax_item = 'X'.
  endif.

*   do not print bases < 0
  if j_1bnfstx-base < 0.
    clear j_1bnfstx-base.
  endif.
  if j_1bnfstx-excbas < 0.
    clear j_1bnfstx-excbas.
  endif.
  if j_1bnfstx-othbas < 0.
    clear j_1bnfstx-othbas.
  endif.

  if ( j_1bnflin-matuse = '2' or                       " BOI note 699257
                           j_1bnflin-matuse = '3' ) and
       intj_1baj-taxgrp = const_taxgrp-icms.
*      consumption material and ICMS tax line
*      => ICMS base must be reported with IPI tax value subtracted
*      => Modelo 1 and NF Writer show different values

    select * from  *j_1bnfstx
           where  docnum  = j_1bnfstx-docnum
           and    itmnum  = j_1bnfstx-itmnum
           and    taxtyp  in ipi_types.
      exit.                             "only one IPI tax line possible
    endselect.

    if sy-subrc = 0.
      if *j_1bnfstx-taxval < j_1bnfstx-othbas.
        subtract *j_1bnfstx-taxval from j_1bnfstx-othbas.
      endif.
    endif.
  endif.                                               " EOI note 699257

* clearing the taxvalues in case of otherbase filled - (note 563271)
  if j_1bnfstx-base   = 0 and
     j_1bnfstx-excbas = 0 and
     j_1bnfstx-othbas > 0.
    clear: j_1bnfstx-taxval.
  endif.

* BOI note 670428: For incoming NF line items with usage 'asset'
* with ICMS value <> 0 and no value in the base amount (the values are
* informed at other base or/and excluded base) => clearing ICMS value
* If only ICMS value <> 0 and no other base filled => ICMS value is
* reported
  if intj_1baj-taxgrp = 'ICMS' and    " ICMS tax line items
     j_1bnfdoc-direct = '1'    and    " of incoming NF with
     ( j_1bnflin-matuse = '3' or          " material usage 'asset'
       j_1bnflin-matuse = '2' ).          " consumption      note 699257

    if j_1bnfstx-base = 0 and
            ( j_1bnfstx-excbas <> 0 or j_1bnfstx-othbas <> 0 ).
      clear j_1bnfstx-taxval.
      clear j_1bnfstx-rate.                                " note 699257
    endif.
  endif.                                            " EOI note 670428

* determination of the record type of the currently processed tax-line-
* item (depending on taxgroup and model), using the table tipo_table

  loop at tipo_table where model = j_1bnfdoc-model
                           and taxgroup = intj_1baj-taxgrp.

    move tipo_table-tipo to it_records-tipo.

* for record 53, 56, 70, 71, 75, 76, 77: exclude cancelled documents, if
* they come from different company; be careful: record 75 is created
* from record 54 => special logic later on.
    if it_records-tipo = '53' or it_records-tipo = '56' or
       it_records-tipo = '70' or it_records-tipo = '71' or
       it_records-tipo = '76' or it_records-tipo = '77'.
      if j_1bnfdoc-cancel = 'X' and j_1bnfdoc-direct = '1'.
        check j_1bnfdoc-entrad = 'X'.  "entradas should appear
      endif.
    endif.

* fill it_records with NF tax values
    move j_1bnfstx-base   to it_records-base.
    move j_1bnfstx-taxval to it_records-taxval.
    move j_1bnfstx-excbas to it_records-excbas.
    move j_1bnfstx-othbas to it_records-othbas.

* note 783404: only incoming NFs model 21/22 are reported in rec. 50/54
*              only outgoing NFs model 21/22 are reported in rec. 76/77
    if j_1bnfdoc-model = '21' or j_1bnfdoc-model = '22'.
      case it_records-tipo.
        when '50'.
          check j_1bnfdoc-direct = 1.
        when '54'.
          check j_1bnfdoc-direct = 1.
        when '76'.
          check j_1bnfdoc-direct = 2.
        when '77'.
          check j_1bnfdoc-direct = 2.
      endcase.
    endif.

    if it_records-tipo = '50'.
      move j_1bnfstx-rate to it_records-rate.
    endif.

* for cancelled nf, only record 50 is reported (values cleared)
    if j_1bnfdoc-cancel = 'X'.
      check it_records-tipo = '50'.

      clear it_records-nftot.
      clear it_records-base.
      clear it_records-stbase.
      clear it_records-taxval.
      clear it_records-excbas.
      clear it_records-othbas.
      clear it_records-nffre.
      clear it_records-nfoth.
      clear it_records-nfins.
      clear it_records-rate.
    endif.

* Creating record 53 according selection: All NFs, inc. NFs or outg. NFs
    if it_records-tipo = '53'.
      if nfdirin = 'X'.
        check j_1bnfdoc-direct = '1'.
      elseif nfdirout = 'X'.
        check j_1bnfdoc-direct = '2'.
      endif.
    endif.

* special treatment of record 54 information: no cumulation of data as
* in the other records; here, each line of a Nota Fiscal must be
* reported separately

    if it_records-tipo = '54'.
* only lines with ICMS will be considered; the respective IPI and
* Substituição tributária information will be added here, if existing
      check intj_1baj-taxgrp = const_taxgrp-icms.    "no IPI without ICMS

      check icms_first_tax_item = 'X'. " allow only 1 ICMS taxline/ item


      move j_1bnfstx-rate to it_records-rate.
      clear it_records-taxval.
      clear it_records-base.

* read additional tax data of this line (only IPI and Sub. trib.)
      select * from *j_1bnfstx where docnum = j_1bnfstx-docnum and
                                     itmnum = j_1bnfstx-itmnum.

        read table intj_1baj with key taxtyp = *j_1bnfstx-taxtyp
                           binary search.
* read IPI data
        if intj_1baj-taxgrp = const_taxgrp-ipi.
          move *j_1bnfstx-taxval to it_records-taxval.
        endif.
* read Substituição tributária data
        if intj_1baj-taxgrp = const_taxgrp-subtrib.
          it_records-stbase = *j_1bnfstx-base + it_records-stbase.
        endif.

        if intj_1baj-taxgrp = const_taxgrp-icms and
           ( *j_1bnfstx-rate = j_1bnfstx-rate or
                                        *j_1bnfstx-rate is initial ).
          it_records-base = it_records-base + *j_1bnfstx-base.
        endif.

      endselect.                       "from *j_1bnfstx

      clear: icms_first_tax_item.
      last_taxrate = j_1bnfstx-rate.

      if assets <> 'X'.                " (note 458935)
        perform fill_tipo_75.
* Exclude assets & consumption from rec. 54 and 75
* previously check on specific CFOPs, now: check of matuse <> '2' or '3'
      elseif ( it_records-matuse <> '2' and it_records-matuse <> '3' ).
        perform fill_tipo_75.                        " note 608129
      endif.

    endif.

* Creating record 56 according selected materials; record 56 is only
* generated for outgoing NFs. Although not explicit specified, record 56
* is only created for NF-model = '01'.
    if it_records-tipo = '56'.
      check not rec_56 is initial.
      check not matnr is initial.                          " note 608129
      check j_1bnflin-matnr in matnr.
      check j_1bnfdoc-direct = 2.
* read additional tax data of this line (only IPI taxrate); BOI nt608129
      select * from *j_1bnfstx where docnum = j_1bnfstx-docnum and
                                     itmnum = j_1bnfstx-itmnum.

        read table intj_1baj with key taxtyp = *j_1bnfstx-taxtyp
                           binary search.
        if intj_1baj-taxgrp = const_taxgrp-ipi.
          move *j_1bnfstx-rate to it_records-rate.
        endif.
      endselect.                                           " note 608129

* BADI for determination of record 56-fields (operation type,
* CGC of concessionaire and chassi)
      call method if_ex_badi_j_1blfa1->determine_rec56_fields
        exporting
          is_j_1bnfdoc = j_1bnfdoc
          is_j_1bnflin = j_1bnflin
        importing
          ev_opertype  = it_records-oper_type
          ev_cgc_conc  = it_records-cgc_conc
          ev_chassi    = it_records-chassi.

    endif.

* record 71 is mandatory only for outgoing process
    if it_records-tipo = '71'.
      check j_1bnfdoc-direct = 2.
    endif.

    if it_records-tipo = '76'.                         " BOI note 608129
      move j_1bnfstx-rate to it_records-rate.
    endif.

    if it_records-tipo = '77'.
      move j_1bnfstx-rate to it_records-rate.
      perform fill_tipo_75.
    endif.                                             " EOI note 608129

* fill zeroes for statistical taxes
    if j_1bnfstx-stattx = 'X'.
      clear: it_records-base,
             it_records-taxval,
             it_records-excbas,
             it_records-othbas.
    endif.

* clear the NF total for ICMS in case of "ignoretotal-flag" is set
    if j_1bnfdoc-cancel <> 'X'.                        " note 941019
      it_records-nftot = j_1binlin-nftot.
    endif.                                             " note 941019
    if intj_1baj-taxgrp = const_taxgrp-icms.
      read table gt_nfitmrule into gs_nfitmrule
           with key itmtyp = j_1bnflin-itmtyp.
      if gs_nfitmrule-ignoretotal = 'X'.
        clear it_records-nftot.
      endif.
    endif.

* only outgoing documents:
* adjust total line value for ICMS-taxes in case of Zona Franca -
    if j_1bnfdoc-direct = '2'.
      if j_1bnfstx-taxtyp = const_taxgrp-iczonaf.
        clear it_records-nftot.
*     reset to j_1binlin-nftot after the extract - in case of other
*     taxes for the same line !
      endif.
    endif.

* now: append the new record to the internal table it_records
    append it_records.
    clear sy-subrc.

  endloop.                             " LOOP AT tipo_table

* readjust rectipo-nftot to j_1binlin-nftot in case of Zona Franca in
* outgoing movements
  if j_1bnfdoc-direct = '2'.
    if j_1bnfstx-taxtyp = const_taxgrp-iczonaf.
      it_records-nftot = j_1binlin-nftot.
    endif.
  endif.

*----------------------------------------------------------------------*
*-----------------------end-of-selection-------------------------------*
*----------------------------------------------------------------------*
end-of-selection.


*----------------------------------------------------------------------*
*------output of errorlog or creation of a table with errorlist--------*
*----------------------------------------------------------------------*

  if sy-pdest ne ' ' and sy-batch = ' '.
    call function 'MESSAGES_GIVE'
      tables
        t_mesg = t_mesg
      exceptions
        others = 1.
  else.
    call function 'MESSAGES_SHOW'
      exporting
        corrections_option    = ' '
        corrections_func_text = ' '
        object                = text-390
      exceptions
        no_messages           = 2.
  endif.

*----------------------------------------------------------------------*
*- Append of tipo 55, 74 and 75 to trigger output in the correct order *
*----------------------------------------------------------------------*
  if only74 is initial.                " note 637576
    it_records-tipo = '55'.
    append it_records.
  endif.                               " note 637576
  if wout74 is initial.                " note 637576
    it_records-tipo = '74'.
    append it_records.
  endif.                               " note 637576
  it_records-tipo = '75'.
  append it_records.

*----------------------------------------------------------------------*
*------------------------------- sort ---------------------------------*
*----------------------------------------------------------------------*
  sort it_records by tipo pstdat series subser nfnum       " note 608129
                      cfop_noext docnum rate matnr.        " note 851428

* begin of note 959823
* Create dummy entry that controls the call of the method
* dummy entry for record 85
  loop at it_records where tipo eq '85'.
    lcl_tabix = sy-tabix.
  endloop.
  if sy-subrc is initial.
    describe table it_records lines sy-tfill.
    lcl_tabix = lcl_tabix + 1.
    clear wa_records.
    wa_records-tipo = '85'.
    if lcl_tabix le sy-tfill.
      insert wa_records into it_records index lcl_tabix.
    else.
      append wa_records to it_records.
    endif.
  endif.
* dummy entry for record 86
  loop at it_records where tipo eq '86'.
    lcl_tabix = sy-tabix.
  endloop.
  if sy-subrc is initial.
    describe table it_records lines sy-tfill.
    lcl_tabix = lcl_tabix + 1.
    clear wa_records.
    wa_records-tipo = '86'.
    if lcl_tabix le sy-tfill.
      insert wa_records into it_records index lcl_tabix.
    else.
      append wa_records to it_records.
    endif.
  endif.
* end of note 959823

*------------------------------------------------------* BOI note 851428
*-- Processing all records besides 54: Summation per CFOP, tax rate ---*
*----------------------------------------------------------------------*
  loop at it_records where tipo <> '54'.
    lcl_tabix = sy-tabix - 1.
    if ( last_record-tipo       = it_records-tipo   and
         last_record-pstdat     = it_records-pstdat and
         last_record-series     = it_records-series and
         last_record-subser     = it_records-subser and
         last_record-nfnum      = it_records-nfnum  and
         last_record-docnum     = it_records-docnum and
         last_record-cfop_noext = it_records-cfop_noext and
         last_record-rate       = it_records-rate ) .
      it_records-nftot  = it_records-nftot  + last_record-nftot.
      it_records-nffre  = it_records-nffre  + last_record-nffre.
      it_records-nfins  = it_records-nfins  + last_record-nfins.
      it_records-nfoth  = it_records-nfoth  + last_record-nfoth.
      it_records-nfdis  = it_records-nfdis  + last_record-nfdis.
      it_records-nfqty  = it_records-nfqty  + last_record-nfqty.
      it_records-base   = it_records-base   + last_record-base.
      it_records-stbase = it_records-stbase + last_record-stbase.
      it_records-excbas = it_records-excbas + last_record-excbas. " CORR
      it_records-othbas = it_records-othbas + last_record-othbas. " CORR
      it_records-taxval = it_records-taxval + last_record-taxval.
      modify it_records index lcl_tabix.
      delete it_records index sy-tabix.
    endif.
    last_record = it_records.
  endloop.                                            " EOI note 851428

*---------------------------------------------------- BOI note 608129 -*
*   NBM has len. 10 with format XXXX.XX.XX => reported with len. 8     *
*- filling matnr with NBM code in record 54 in case of initial matnr --*
*----------------------------------------------------------------------*
  loop at it_records where tipo = '54'.
    replace '.' with ' ' into it_records-nbm.
    replace '.' with ' ' into it_records-nbm.
    condense it_records-nbm no-gaps.
    shift it_records-nbm left deleting leading '0'.
    if it_records-matnr is initial.
      it_records-matnr = it_records-nbm.
    endif.
    modify it_records.
  endloop.                                            " EOI note 608129

*------------------------------------------------------* BOI note 838208
*----- Processing record 54, item number 991, 992 and 999 -------------*
*----------------------------------------------------------------------*
  clear: rectipo54-matnr,
         rectipo54-linnfnet,
         rectipo54-icmsbase,
         rectipo54-icstbase,
         rectipo54-stxtaxval.
  loop at it_records where tipo = '54'.
    lcl_tabix = sy-tabix - 1.
    if not it_records-nffre is initial.
      it_records-itmnum = '991'.
      insert it_records index sy-tabix.
    endif.
    if it_records-nfins <> 0.
      it_records-itmnum = '992'.
      insert it_records index sy-tabix.
    endif.
    if it_records-nfoth <> 0.
      it_records-itmnum = '999'.
      insert it_records index sy-tabix.
    endif.
  endloop.

  loop at it_records into wa_records where tipo      = '54'
                                       and itmnum(5) = '00099'.
    lcl_tabix = sy-tabix.

    loop at it_records where tipo = '54'
                         and docnum     = wa_records-docnum
                         and itmnum     = wa_records-itmnum.
      if sy-subrc = 0.
        rec54sum-freight   = rec54sum-freight    +  it_records-nffre.
        rec54sum-insurance = rec54sum-insurance  +  it_records-nfins.
        rec54sum-expenses  = rec54sum-expenses   +  it_records-nfoth.
        delete table it_records.
      endif.
    endloop.
    wa_records-nffre = rec54sum-freight.
    wa_records-nfins = rec54sum-insurance.
    wa_records-nfoth = rec54sum-expenses.
    append wa_records  to it_records_99.                 " note 851428
    clear : rec54sum-expenses, rec54sum-insurance,
    rec54sum-freight.                                    " note 863634
  endloop.
  append lines of it_records_99 to it_records.           " note 851428

*-----------------------------------------------------*  BOI note 608129
*-- remove records 50 for cancelled NFs if they are included again   --*
*----------------------------------------------------------------------*
  loop at it_records into wa_records where tipo = '50'
                                       and situation ca 'SX'.
    read table it_records with key tipo = '50'
*                             pstdat     = wa_records-pstdat "nt. 637576
                             docdat     = wa_records-docdat "nt. 637576
                             nfnum      = wa_records-nfnum
                             series     = wa_records-series
                             subser     = wa_records-subser
*                             cfop_noext = wa_records-cfop_noext "637576
*                             itmnum     = wa_records-itmnum "nt. 637576
*                             matnr      = wa_records-matnr  "nt. 637576
                             parid      = wa_records-parid  "nt. 637576
                             cancel     = ' '.
    if sy-subrc = 0.
      delete table it_records from wa_records.
    endif.
  endloop.                             " EOI note 608129

*-----------------------------------------------------*  BOI note 608129
*-- remove records 54 when (1) "asset-flag is set and (2) material ----*
*-- usage is 'asset' or 'consumption' ---------------------------------*
*----------------------------------------------------------------------*
  if not assets is initial.
    loop at it_records into wa_records where tipo = '54'.
      if wa_records-matuse = '2' or wa_records-matuse = '3'.
        delete table it_records from wa_records.
      endif.
    endloop.
  endif.                                            " EOI note 608129

*------------------------------------------------------* BOI note 838208
* Sorting of record 54 according CGC, serie, subserie, NF number, itmnum
*----------------------------------------------------------------------*
  loop at it_records into wa_records where tipo = '54'.
    append wa_records to it_records54.
    delete table it_records from wa_records.
  endloop.
  sort it_records54 by cgc series subser nfnum itmnum.
  loop at it_records into wa_records where tipo > '54'.
    insert lines of it_records54 into it_records.
    exit.
  endloop.                                            " EOI note 838208

*----------------------------------------------------------------------*
*-------------------------------- loop --------------------------------*
*----------------------------------------------------------------------*
  loop at it_records.
    lcl_tabix = sy-tabix.

*----------------------------------------------------------------------*
*-------------------------- at new key-rate ---------------------------*
*----------------------------------------------------------------------*
* 'at new itmnum' is activated if the itmnum or if any field of key
* standing left of itmnum changes
    at new itmnum.                                        " note 851428
      read table it_records index lcl_tabix.
      perform trigger_output.
      clear taxsums.
    endat.

*----------------------------------------------------------------------*

    move it_records-tipo to tipo.

* depending on the type (tipo) of record, fill the respective
* output structure
* a special treatment has to be implemented for records of type 54:
* no cumulation of data as in the other records (i.e. no use of the
* 'taxsums' variables) - this means: each line of a Nota Fiscal must be
* reported separately; however, each material number can only appear
* once (i.e. 2 items on a NF with the same material (and with the same
* CFOP/tax rate) appear as one record

    case tipo.

      when '50'.
        move-corresponding: it_records     to rectipo50.
        perform fill_nfserie using rectipo50-series
                                   it_records-series
                                   it_records-subser.
        rectipo50-rate        = it_records-rate * 100.

      when '51'.
        move-corresponding: it_records     to rectipo51.
        perform fill_nfserie using rectipo51-series
                                   it_records-series
                                   it_records-subser.

      when '53'.
        move-corresponding: it_records      to rectipo53.
        perform fill_nfserie using rectipo53-series      " note 608129
                           it_records-series             " note 608129
                           it_records-subser.            " note 608129

      when '54'.
* Exclude assets & consumption from rec. 54 and 75
* previously check on specific CFOPs, now: check of matuse <> '2' or '3'
*        IF assets <> 'X' OR                               " note 608129
*        it_records-matuse <> '2' AND it_records-matuse <> '3'."nt608129
        move-corresponding: it_records   to rectipo54.
        perform fill_nfserie using rectipo54-series   " note 608129
                       it_records-series              " note 608129
                       it_records-subser.             " note 608129

* material number in R/3 has length 18 => reported with length 14
*        PERFORM condense_matnr USING it_records-matnr matnr_short.
* BADI for processing material number
        call method if_ex_badi_j_1blfa1->process_material_number"nt660429
              exporting
                is_matnr            = it_records-matnr
              importing
                ev_matnr            = matnr_short.

        if not matnr_short is initial.
          rectipo54-matnr = matnr_short.                " note 608129
        else.
          rectipo54-matnr = it_records-matnr.
        endif.

** NBM in R/3 has len. 10 with format XXXX.XX.XX => reported with len. 8
*          PERFORM condense_nbm USING it_records-nbm tmp_nbm. "BOD608129
*
** fill material number with NBM in case of initial material number
*          IF matnr_short = ''.
*            rectipo54-matnr = tmp_nbm.
*          ELSE.
*            rectipo54-matnr = matnr_short.
*          ENDIF.                                       "EOD note 608129
* remove decimal points according to rules given by law
*        rectipo54-linnftot  = it_records-nftot  * 100.   " note 762594
        rectipo54-linnfnet  = it_records-nfnet  * 100.    " note 762594
        rectipo54-stxtaxval = it_records-taxval * 100.
        rectipo54-icmsbase  = it_records-base   * 100.
        rectipo54-icstbase  = it_records-stbase * 100.
        rectipo54-rate      = it_records-rate   * 100.
        rectipo54-nfqty     = it_records-nfqty  * 1000.
        rectipo54-discount  = it_records-nfdis  * 100.
        rectipo54-freight   = it_records-nffre  * 100.
        rectipo54-insurance = it_records-nfins  * 100.
        rectipo54-expenses  = it_records-nfoth  * 100.
*        ENDIF.                                        "note 608129


      when '56'.
        move-corresponding: it_records     to rectipo56.
        perform fill_nfserie using rectipo56-series
                                   it_records-series
                                   it_records-subser.
        rectipo56-rate      = it_records-rate   * 100.

      when '55'.
        perform fill_tipo_55.

      when '70'.
        move-corresponding: it_records     to rectipo70.

      when '71'.
        move-corresponding: it_records to rectipo71.
        perform read_reference_nf.

      when '74'.
        perform fill_tipo_74.

      when '75'.
        perform output75.

      when '76'.
        move-corresponding: it_records to rectipo76.

      when '77'.
        move-corresponding: it_records to rectipo77.
*                                                       " note 608129
*        PERFORM condense_matnr USING it_records-matnr matnr_short.
* BADI for processing material number
        call method if_ex_badi_j_1blfa1->process_material_number"nt660429
              exporting
                is_matnr            = it_records-matnr
              importing
                ev_matnr            = matnr_short.

        if matnr_short is initial.
          matnr_short = it_records-matnr.
        endif.

        if matnr_short+6 = 'SINTEG'.
          concatenate matnr_short(3) matnr_short+6(8)
                                              into rectipo77-matnr.
        else.
          rectipo77-matnr   = matnr_short.
        endif.
        perform condense_nbm using it_records-nbm tmp_nbm.
        if rectipo77-matnr = ''.
          rectipo77-matnr = tmp_nbm.
        endif.                                         " note 608129

      when '85'.                                     " Begin note 862655
        if not it_records-docnum is initial.                "note 959823
          clear lv_ind_ignore85.                              "note 959823

          move-corresponding: it_records  to rectipo85.

          call method if_ex_badi_j_1blfa1->export_processing
            exporting
              iv_docnum         = it_records-docnum
            importing
              ev_de_number      = rectipo85-de_number
              ev_de_date        = rectipo85-de_date
              ev_export_reason  = rectipo85-export_reason
              ev_export_regist  = rectipo85-export_regist
              ev_regist_date    = rectipo85-regist_date
              ev_conheci_numb   = rectipo85-conheci_numb
              ev_conheci_date   = rectipo85-conheci_date
              ev_conheci_type   = rectipo85-conheci_type
              ev_country        = rectipo85-country
              ev_filler         = rectipo85-filler1
              ev_averbacao_date = rectipo85-averbacao_date
              ev_relationship   = rectipo86-relation
* begin of note 959823
              ev_indicator      = lv_ind_ignore85.

* Indicator lv_ind_ignore85 decides if record 85 is created
          if not lv_ind_ignore85 is initial.
            clear rectipo85.
          endif.

        else.

* Reading of internal table through EXPORT_PROCESSING_ADD85
          call method if_ex_badi_j_1blfa1->export_processing_add85
            importing
              et_rec85 = lt_rec85.

          refresh rectipo85.

          loop at lt_rec85 into ls_rec85.
            move-corresponding ls_rec85 to rectipo85.
            rectipo85-export_regist = ls_rec85-re_number.
            rectipo85-regist_date   = ls_rec85-re_date.
            rectipo85-tipo = '85'.
            append rectipo85.
          endloop.

          loop at rectipo85.
            count85 = count85 + 1.
            perform output using rectipo85.
          endloop.
          clear rectipo85.

        endif.
* end of note 959823

      when '86'.
        if not it_records-docnum is initial.                "note 959823
          clear lv_ind_ignore86.                              "note 959823

          move-corresponding: it_records  to rectipo86.
*        MOVE-CORRESPONDING: rectipo85   TO rectipo86. " DEL note 943553
          " BOI note 943553
          call method if_ex_badi_j_1blfa1->export_processing
            exporting
              iv_docnum    = it_records-docnum
            importing
              ev_de_number = rectipo85-de_number
              ev_de_date   = rectipo85-de_date
              ev_indicator = lv_ind_ignore86.                 "note 959823

          rectipo86-de_number   = rectipo85-de_number.
          rectipo86-de_date     = rectipo85-de_date.     " EOI note 943553
          " End note 862655
* begin of note 959823
* Indicator lv_ind_ignore86 decides if record 86 is created
          if not lv_ind_ignore86 is initial.
            clear rectipo86.
          endif.

        else.

* Reading of internal table through EXPORT_PROCESSING_ADD86
          call method if_ex_badi_j_1blfa1->export_processing_add86
            importing
              et_rec86 = lt_rec86.

          refresh rectipo86.

          loop at lt_rec86 into ls_rec86.
            move-corresponding ls_rec86 to rectipo86.
            rectipo86-de_number = ls_rec86-re_number.
            rectipo86-de_date   = ls_rec86-re_date.
            rectipo86-tipo = '86'.
            clear matnr_short.
            call method if_ex_badi_j_1blfa1->process_material_number
              exporting
                is_matnr = ls_rec86-matnr
              importing
                ev_matnr = matnr_short.
            if matnr_short is initial.
              move ls_rec86-matnr to rectipo86-matnr.
            else.
              move matnr_short to rectipo86-matnr.
            endif.
            append rectipo86.
          endloop.

          loop at rectipo86.
            count86 = count86 + 1.
            perform output using rectipo86.
          endloop.
          clear rectipo86.

        endif.
* end of note 959823

    endcase.                           "case it_records-tipo

    taxsums-linnfnet   = taxsums-linnfnet   + it_records-nfnet." 762594
    taxsums-linnftot   = taxsums-linnftot   + it_records-nftot.
    taxsums-stxbase    = taxsums-stxbase    + it_records-base.
    taxsums-stxstbase  = taxsums-stxstbase  + it_records-stbase.
    taxsums-stxtaxval  = taxsums-stxtaxval  + it_records-taxval.
    taxsums-stxexcbase = taxsums-stxexcbase + it_records-excbas.
    taxsums-stxothbase = taxsums-stxothbase + it_records-othbas.
    taxsums-expenses   = taxsums-expenses   + it_records-nffre
                                            + it_records-nfoth
                                            + it_records-nfins.

  endloop.

*----------------------------------------------------------------------*
*------------- write last record coming from it_records ---------------*
*----------------------------------------------------------------------*
  perform trigger_output.


* fill the record type 90 with company-data (same as in record
* type 10) and the numbers of the previously issued records type 50 to
* type 70 and the total of all the records (incl. type 10 and type 90)
  countall = count50 + count51 + count53 + count54 + count55 + count56
      + count70 + count71 + count74 + count75 + count76 + count77 + 3
      + count85 + count86.                                 " note 943553

  perform determine_record_number using count50 recordnumb.
  perform determine_record_number using count51 recordnumb.
  perform determine_record_number using count53 recordnumb.
  perform determine_record_number using count54 recordnumb.
  perform determine_record_number using count55 recordnumb.
  perform determine_record_number using count56 recordnumb.
  perform determine_record_number using count70 recordnumb.
  perform determine_record_number using count71 recordnumb.
  perform determine_record_number using count74 recordnumb.
  perform determine_record_number using count75 recordnumb.
  perform determine_record_number using count76 recordnumb.
  perform determine_record_number using count77 recordnumb.
  perform determine_record_number using count85 recordnumb. " nt. 862655
  perform determine_record_number using count86 recordnumb. " nt. 862655

  if recordnumb > 8. countall = countall + 1. endif.       " note 608129

  move: '90'                   to rectipo90-tipo,
        cgc_number             to rectipo90-cgc,
        branch_data-state_insc to rectipo90-state_insc.
  if count50 ne 0.
    move: '50' to rectipo90-numtip50,
          count50 to rectipo90-count50.
  endif.
  if count51 ne 0.
    move: '51' to rectipo90-numtip51,
          count51 to rectipo90-count51.
  endif.
  if count53 ne 0.
    move: '53' to rectipo90-numtip53,
          count53 to rectipo90-count53.
  endif.
  if count54 ne 0.
    move: '54' to rectipo90-numtip54,
          count54 to rectipo90-count54.
  endif.
  if count55 ne 0.
    move: '55' to rectipo90-numtip55,
          count55 to rectipo90-count55.
  endif.
  if count56 ne 0.
    move: '56' to rectipo90-numtip56,
          count56 to rectipo90-count56.
  endif.
  if count70 ne 0.
    move: '70' to rectipo90-numtip70,
          count70 to rectipo90-count70.
  endif.
  if count71 ne 0.
    move: '71' to rectipo90-numtip71,
          count71 to rectipo90-count71.
  endif.
  if count74 ne 0.
    move: '74' to rectipo90-numtip74,
          count74 to rectipo90-count74.
  endif.
  if count75 ne 0.
    move: '75' to rectipo90-numtip75,
          count75 to rectipo90-count75.
  endif.
  if count76 ne 0.
    move: '76' to rectipo90-numtip76,
          count76 to rectipo90-count76.
  endif.
  if count77 ne 0.
    move: '77' to rectipo90-numtip77,
          count77 to rectipo90-count77.
  endif.
  if count85 ne 0.                          " Begin note 862655
    move: '85' to rectipo90-numtip85,
          count85 to rectipo90-count85.
  endif.
  if count86 ne 0.
    move: '86' to rectipo90-numtip86,
          count86 to rectipo90-count86.
  endif.                                      " End note 862655
  move:   '99'      to rectipo90-numtip99,                 " note 608129
          countall  to rectipo90-countall.                 " note 608129

*  When fiels are empty ('9999999999'), they are filled with blanks
  clear sy-subrc.
  while sy-subrc is initial.
    replace '9999999999' with '          ' into rectipo90.
  endwhile.

*  Blanks in part2 of rectipo90b (after CGC) are deleted & '1' is added
*  part2 = rectipo90+30(120).                              " note 608129
*  part2 = rectipo90+30(130).           " note 608129
  part2 = rectipo90+30(150).           " note 862655

  condense part2 no-gaps.
  clear rectipo90+30(96).
*  if recordnumb < 10.                                     " note 608129
  if recordnumb < 9.                   " note 608129
    rectipo90+30(110) = part2.
    rectipo90+125(1) = 1.
    perform output using rectipo90.
  else.
    rectipo90+30(90) = part2.
    rectipo90+125(1) = 2.
    perform output using rectipo90.
    clear rectipo90+30(95).
*    rectipo90+30(30) = part2+90.                          " note 608129
*    rectipo90+30(40) = part2+90.       " note 608129
    rectipo90+30(60) = part2+90.       " note 862655
    perform output using rectipo90.
  endif.

* move data from internal table to the file
  if testrun is initial.
    if applserv = 'X'.
      perform open_file_a.
      perform transfer_data.
      perform close_file_a.
    elseif presserv = 'X'.
      concatenate 'tmp' sy-datum sy-uzeit into file_a.
      perform open_file_a.
      perform transfer_data.
      perform close_file_a.

      file_p = file_p_help.                                " note 716358
      call function 'ARCHIVFILE_SERVER_TO_CLIENT'
        exporting
          path       = file_a
          targetpath = file_p
        exceptions
          error_file = 1
          others     = 2.

      if sy-subrc <> 0.
      endif.

    endif.
  endif.

*----------------------------------------------------------------------*
*------------------------ output errorlist ----------------------------*
*----------------------------------------------------------------------*

  new-page.

  loop at t_mesg.
    write / t_mesg.
  endloop.

*----------------------------------------------------------------------*
*-------------------- Screenoutput of record 90 -----------------------*
*----------------------------------------------------------------------*

  write: / text-200, '10', at 30 text-201, at 60 '1'.
  write: / text-200, '11', at 30 text-201, at 60 '1'.
  write: / text-200, '50', at 30 text-201, count50.
  write: / text-200, '51', at 30 text-201, count51.
  write: / text-200, '53', at 30 text-201, count53.
  write: / text-200, '54', at 30 text-201, count54.
  write: / text-200, '55', at 30 text-201, count55.
  write: / text-200, '56', at 30 text-201, count56.
  write: / text-200, '70', at 30 text-201, count70.
  write: / text-200, '71', at 30 text-201, count71.
  write: / text-200, '74', at 30 text-201, count74.
  write: / text-200, '75', at 30 text-201, count75.
  write: / text-200, '76', at 30 text-201, count76.
  write: / text-200, '77', at 30 text-201, count77.
  write: / text-200, '85', at 30 text-201, count85.        " note 862655
  write: / text-200, '86', at 30 text-201, count86.        " note 862655
*  if recordnumb < 10.                                     " note 608129
  if recordnumb < 9.                   " note 608129
    write: / text-200, '90', at 30 text-201, at 60 '1'.
  else.
    write: / text-200, '90', at 30 text-201, at 60 '2'.
  endif.
  skip.
  write: / text-202,  at 51 countall.
  skip.

  if not applserv is initial.
    write:/ text-500.                  " The file has been downloaded to
    write:/ file_a.
  elseif not presserv is initial.
    write:/ text-500.                  " The file has been downloaded to
    write:/ file_p.
  elseif not testrun is initial.
    write:/ text-505. " You have only executed a test run ......
    write:/ text-506. " In order to generate a file, rerun .....
    write:/ text-507. " and in "File name," enter the path, ....
  endif.

*----------------------------------------------------------------------*
*---------------------form fill_tipo_table-----------------------------*
*----------------------------------------------------------------------*
*------------fills the internal table tipo_table-----------------------*
*----------------------------------------------------------------------*
*  --> model, group, type: model numbers of fiscal documents,
*                          taxgropups, recordtypes of the arquivo
*  <-- tipo_table        : table that assigns the respective output
*                          recordtypes of the Arquivo Magnético to
*                          model and taxgroup
*----------------------------------------------------------------------*
form fill_tipo_table using model like j_1bnfdoc-model
                           group like j_1baj-taxgrp
                           type  like j_1blfam10-tipo.

  move model to tipo_table-model.
  move group to tipo_table-taxgroup.
  move type  to tipo_table-tipo.

  append tipo_table.

endform.                               "fill_tipo_table

*----------------------------------------------------------------------*
*-----------------------Form  TRIGGER_OUTPUT---------------------------*
*----------------------------------------------------------------------*
*-------fills correct output record according to recordtype tipo-------*
*---------------and triggers output of this record---------------------*
*----------------------------------------------------------------------*
*  --> tipo, taxsums, count50 - count70, rectipo50 - rectipo70:
*      output fields
*  <-- form output
*----------------------------------------------------------------------*
form trigger_output.

  if  not rectipo50 is initial or
      not rectipo51 is initial or
      not rectipo53 is initial or
      not rectipo54 is initial or
      not rectipo56 is initial or
      not rectipo70 is initial or
      not rectipo71 is initial or
      not rectipo76 is initial or
*      NOT rectipo77 IS INITIAL.      " Begin note 862655
      not rectipo77 is initial or
      not rectipo85 is initial or
      not rectipo86 is initial.         " End note 862655


    perform convert_decimals changing taxsums.
    case tipo.
      when '50'.
        count50 = count50 + 1.
        move-corresponding taxsums to rectipo50.
        perform output using rectipo50.

      when '51'.
        count51 = count51 + 1.
        move-corresponding taxsums to rectipo51.
        perform output using rectipo51.

      when '53'.
        count53 = count53 + 1.
        move-corresponding taxsums to rectipo53.
        perform output using rectipo53.

      when '54'.
        count54 = count54 + 1.
        if rectipo54-itmnum(2) <> '99'.                    " note 838208
* sequential numbering for record 54.                  " boi note 863634
          if previous_nfnum is initial.
            previous_nfnum = rectipo54-nfnum.
          endif.
          if rectipo54-nfnum = previous_nfnum.
            countrectipo54    = countrectipo54 + 1.
          else.
            countrectipo54   = 1.
          endif.
          previous_nfnum = rectipo54-nfnum.
          rectipo54-itmnum = countrectipo54.           " eoi note 863634
          perform output using rectipo54.
        endif.                                             " note 838208
        if rectipo54-itmnum = '990'.                   " boi note 863634
          countrectipo54    = countrectipo54 + 1.
          rectipo54-itmnum = countrectipo54.
          previous_nfnum = rectipo54-nfnum.
          perform output using rectipo54.
        endif.                                         " eoi note 863634
*        IF NOT rectipo54-freight IS INITIAL.              " note 838208
        if rectipo54-itmnum = '991'.                       " note 838208
          perform output_add_rec54 using '991' rectipo54-freight.
        endif.
*        IF NOT rectipo54-insurance IS INITIAL.            " note 838208
        if rectipo54-itmnum = '992'.                       " note 838208
          perform output_add_rec54 using '992' rectipo54-insurance.
        endif.
*        IF NOT rectipo54-expenses IS INITIAL.             " note 838208
        if rectipo54-itmnum = '999'.                       " note 838208
          perform output_add_rec54 using '999' rectipo54-expenses.
        endif.

      when '56'.
        count56 = count56 + 1.
        lastrectipo56    = lastrectipo56 + 1.            " note 608129
        rectipo56-itmnum = lastrectipo56.                " note 608129
        move-corresponding taxsums to rectipo56.
        perform output using rectipo56.

      when '70'.
        if allop = 'X'.
          count70 = count70 + 1.
          move-corresponding taxsums to rectipo70.
          perform output using rectipo70.
        endif.

      when '71'.
        if allop = 'X'.
          count71 = count71 + 1.
          perform output using rectipo71.
        endif.

      when '76'.
        count76 = count76 + 1.
        move-corresponding taxsums to rectipo76.
        perform output using rectipo76.

      when '77'.
        count77 = count77 + 1.
        lastrectipo77    = lastrectipo77 + 1.            " note 608129
        rectipo77-itmnum = lastrectipo77.                " note 608129
        move-corresponding taxsums to rectipo77.
        perform output using rectipo77.

      when '85'.                                  " Begin note 862655
        check not rectipo85 is initial.                  "note 959823
        count85 = count85 + 1.
        perform output using rectipo85.

      when '86'.
        check not rectipo86 is initial.                  "note 959823
        count86 = count86 + 1.
        perform output using rectipo86.             " End note 862655

      when others.

    endcase.

  endif.

endform.                               " TRIGGER_OUTPUT

*---------------------------------------------------------------------*
*---------------------form convert_decimals---------------------------*
*---------------------------------------------------------------------*
*------------------removes the decimal point--------------------------*
*---------------------------------------------------------------------*
*  --> taxsums: taxsums with decimal points
*  <-- taxsums: taxsums without decimal points (*100)
*----------------------------------------------------------------------*
form convert_decimals changing value(convert_help) structure taxsums.

  convert_help-linnfnet   =  convert_help-linnfnet   * 100. " nt 762594
  convert_help-linnftot   =  convert_help-linnftot   * 100.
  convert_help-stxbase    =  convert_help-stxbase    * 100.
  convert_help-stxtaxval  =  convert_help-stxtaxval  * 100.
  convert_help-stxexcbase =  convert_help-stxexcbase * 100.
  convert_help-stxothbase =  convert_help-stxothbase * 100.
  convert_help-expenses   =  convert_help-expenses   * 100.

endform.                               "convert_decimals

*---------------------------------------------------------------------*
*--------------------------form output--------------------------------*
*---------------------------------------------------------------------*
* performs output on screen or file depending on the parameter testrun*
*---------------------------------------------------------------------*
*  --> record, recordname
*  <-- output on screen or file
*----------------------------------------------------------------------*
form output using record.

  case testrun.
    when 'X'.         "only testrun = output on screen; no file creation
      write / record.
    when ' '.                          "only creation of a file
      append record to it_record.
  endcase.

endform.                               "OUTPUT

*&---------------------------------------------------------------------*
*&      Form  FORMAT_STAINS
*&---------------------------------------------------------------------*
*   convert state inscription to file format - numbers only
*----------------------------------------------------------------------*
*  -->  unformatted state inscription, might contain non numerical data
*  <--  formatted state inscription, contains only numerical data
*----------------------------------------------------------------------*
form format_stains using value(st_in) like parnad-stains
                  changing value(st_out) type c.

  data: st_wk(1) type c.
  clear st_out.

  shift st_in left deleting leading space.
  do 18 times.
    st_wk = st_in.
    if st_wk ca '0123456789'.
      concatenate st_out st_wk into st_out.
    endif.
    shift st_in left circular.
  enddo.

endform.                               " FORMAT_STAINS

*&---------------------------------------------------------------------*
*&      Form  CLOSE_FILE
*&---------------------------------------------------------------------*
form close_file_a.

*  IF testrun = ' ' AND applserv = 'X'.          " note 637576
  if testrun = ' '.                              " note 637576
    close dataset file_a.
  endif.

endform.                               " CLOSE_FILE

*&---------------------------------------------------------------------*
*&      Form  TRANSFER_DATA
*&---------------------------------------------------------------------*
form transfer_data.
  loop at it_record.
    transfer it_record to file_a length 126.
    if sy-subrc ne 0.
      message e458.
    endif.
  endloop.

endform.                               " TRANSFER_DATA

*&---------------------------------------------------------------------*
*&      Form  OPEN_FILE
*&---------------------------------------------------------------------*
form open_file_a.

  if testrun = ' '.                    " AND APPLSERV = 'X'.
    open dataset file_a for output in text mode
    encoding non-unicode ignoring conversion errors.
    if sy-subrc ne 0.
      message e452 with file_a.
    endif.
  endif.

endform.                               " OPEN_FILE

*&---------------------------------------------------------------------*
*&      Form  fill_tipo_74
*&---------------------------------------------------------------------*
*       Generation of the inventory record 74
*----------------------------------------------------------------------*
form fill_tipo_74.

* prepare rectipo75 that each material is unique
  perform modify_material_table.                      " note 637576

* filling the internal material from records of record75
  select matnr mtart meins from mara into corresponding fields
    of table material for all entries in rectipo75
          where matnr = rectipo75-matnr_long.
* Loop over all materials
  loop at material.
    perform prepare_it_msku.  " read msku independent of period/plant
    perform prepare_it_mslb.  " read mslb independent of period/plant
    clear: summe, rectipo74, it_consig_summe. " clear all headers
    refresh: it_consig_summe.
    move '74'   to rectipo74-tipo.
    loop at plants.
      perform get_inventory.
    endloop.                         "plants
    perform report_summe_rec74.
  endloop.                           " LOOP AT material.
  perform output74.
endform.                               " fill_tipo_74

*&---------------------------------------------------------------------*
*&      Form  FILL_TIPO_75
*&---------------------------------------------------------------------*
form fill_tipo_75.

  clear rectipo75.
  rectipo75-tipo      = '75'.
  rectipo75-inidate   = j5_pdate-low.  "initial date
  rectipo75-finaldate = j5_pdate-high. "final date

* material number in R/3 has length 18 => reported with length 14
*  PERFORM condense_matnr USING j_1bnflin-matnr matnr_short.
* BADI for processing material number
  call method if_ex_badi_j_1blfa1->process_material_number  "nt660429
    exporting
      is_matnr            = j_1bnflin-matnr
    importing
      ev_matnr            = matnr_short.

  if matnr_short is initial.
    matnr_short = j_1bnflin-matnr.
  endif.

* NBM in R/3 has len. 10 with format XXXX.XX.XX => reported with len. 8
  perform condense_nbm using j_1bnflin-nbm tmp_nbm.

* note 508192: Fill rectipo75 always! I.e. just before output adjacent
* duplicates are deleted; in case of NF without matnr (only description)
* the field matnr is also filled with the NBM code
  if not matnr_short is initial.
    rectipo75-matnr = matnr_short.
  else.
    rectipo75-matnr = tmp_nbm.
  endif.

  rectipo75-nbm        = tmp_nbm.
  rectipo75-nbm_orig   = j_1bnflin-nbm.                    " note 608129
  rectipo75-decript    = j_1bnflin-maktx.
  rectipo75-meins      = j_1bnflin-meins.
*  rectipo75-st_code(1) = j_1bnflin-matorg.  " note 608129   note 720863
*  rectipo75-matorg     = j_1bnflin-matorg.                " note 608129
  if not j_1bnflin-matnr is initial.                       " note 608129
    rectipo75-matnr_long = j_1bnflin-matnr.
  else.                                                    " note 608129
    rectipo75-matnr_long = tmp_nbm.                        " note 608129
  endif.                                                   " note 608129
  rectipo75-direct     = j_1bnfdoc-direct.
  rectipo75-entrad     = j_1bnfdoc-entrad.
  rectipo75-cancel     = j_1bnfdoc-cancel.                 " note 608129

*   'write' instead of 'move', because of conversion routine
*  WRITE j_1bnflin-taxsit  TO rectipo75-taxsit.            " note 608129
*  WRITE j_1bnflin-taxsit  TO rectipo75-st_code+1(2).      " note 720863
* note 544652: reporting of the sales unit, if sales unit of measure and
* purchase unit differ and incoming and outgoing NFs are found for
* selection criteria. Therefore: inclusion of a sorting parameter, which
* brings the record with the sales unit in a earlier position of
* rectipo75 after the SORT-command and which will survive afer the
* DELETE ADJACENT DUPLICATES-command
  if j_1bnfdoc-direct = '1'.
    rectipo75-sort_pur_sal = '2'.
  else.
    rectipo75-sort_pur_sal = '1'.
  endif.
  append rectipo75.

endform.                               " FILL_TIPO_75

*&---------------------------------------------------------------------*
*&      Form  OUTPUT74
*&---------------------------------------------------------------------*
form output74.
  loop at rectipo74.
    count74 = count74 + 1.
    perform output using rectipo74.
  endloop.
endform.                                                    " OUTPUT74

*&---------------------------------------------------------------------*
*&      Form  OUTPUT75
*&---------------------------------------------------------------------*
form output75.

  constants comp_nine(20) type c value '09182736455463728190'."BOI 608129
  data:     inverted_date like j_1btxip1-validfrom,
            lt_txip1   like j_1btxip1   occurs 0 with header line,
            lt_txip2   like j_1btxip2   occurs 0 with header line,
            lt_txic1   like j_1btxic1   occurs 0 with header line,
            lt_txic2   like j_1btxic2   occurs 0 with header line,
            lt_txgruop like j_1btxgruop occurs 0 with header line,
            lt_txip3   like j_1btxip3   occurs 0 with header line,
            lt_txic3   like j_1btxic3   occurs 0 with header line,
            lt_txst1   like j_1btxst1   occurs 0 with header line.

* when rec 74 not processed: prepare rec 75 that each material is unique
  if not wout74 is initial.                                " note 637576
    perform modify_material_table.                         " note 637576
  endif.                                                   " note 637576

* invert date
  move rep_date_high to inverted_date.
  translate inverted_date using comp_nine.

* read IPI taxrate into internal tables
  check not rectipo75[] is initial.
  select * from j_1btxip1 into table lt_txip1
      for all entries in rectipo75
      where nbmcode = rectipo75-nbm_orig
        and validfrom gt inverted_date.
  sort lt_txip1 by nbmcode validfrom.
  delete adjacent duplicates from lt_txip1 comparing nbmcode.

  select * from j_1btxip2 into table lt_txip2
      for all entries in rectipo75
      where matnr = rectipo75-matnr_long
        and validfrom gt inverted_date.
  sort lt_txip2 by matnr validfrom.
  delete adjacent duplicates from lt_txip2 comparing matnr.

  select * into corresponding fields of table lt_txip3
              from j_1btxip3 as a inner join j_1btxgruop as b
              on a~gruop = b~gruop
              where ( b~field = 'MATNR' or b~field = 'NBM' )
                and a~validfrom gt inverted_date.
  sort lt_txip3 by value validfrom.
  delete adjacent duplicates from lt_txip3 comparing value.

* read ICMS taxrate into internal tables
  select * from j_1btxic1 into table lt_txic1
      where land1    = address-country
        and shipfrom = address-region
        and shipto   = address-region
        and validfrom gt inverted_date.
  sort lt_txic1 by validfrom.

  select * from j_1btxic2 into table lt_txic2
    for all entries in rectipo75
     where land1    = address-country
       and shipfrom = address-region
       and shipto   = address-region
       and matnr    = rectipo75-matnr_long
       and validfrom gt inverted_date
       and validto   lt inverted_date.
  sort lt_txic2 by matnr validfrom.
  delete adjacent duplicates from lt_txic2 comparing matnr.

  select * into corresponding fields of table lt_txic3
              from j_1btxic3 as a inner join j_1btxgruop as b
              on a~gruop = b~gruop
              where b~field    = 'MATNR'
                and a~land1    = address-country
                and a~shipfrom = address-region
                and a~shipto   = address-region
                and a~validfrom gt inverted_date
                and a~validto   lt inverted_date.
  sort lt_txic3 by value validfrom.
  delete adjacent duplicates from lt_txic3 comparing value.

* read ST taxbase into internal table
  select * from j_1btxst1 into table lt_txst1
    for all entries in rectipo75
     where land1    = address-country
       and shipfrom = address-region
       and shipto   = address-region
       and matnr    = rectipo75-matnr_long
       and validfrom gt inverted_date
       and validto   lt inverted_date.
  sort lt_txst1 by matnr validfrom.
  delete adjacent duplicates from lt_txst1 comparing matnr.

  loop at rectipo75.
* fill IPI taxrate with values from j_1btxip1/j_1btxip2
*    read table lt_txip1 with key nbmcode = rectipo75-nbm_orig."nt720863
*    if sy-subrc = 0.
*      rectipo75-ipirate = lt_txip1-rate * 100.
*    else.
*      read table lt_txip2 with key matnr = rectipo75-matnr_long.
*      if sy-subrc = 0.
*        rectipo75-ipirate = lt_txip2-rate * 100.
*      else.
*        loop at lt_txip3.
*          if ( lt_txip3-value = rectipo75-nbm_orig
*                            or lt_txip3-value = rectipo75-matnr_long ).
*            rectipo75-ipirate = lt_txip3-rate * 100.
*            exit.
*          endif.
*        endloop.
*      endif.
*    endif.                                       " EOD corr note 720863
* BOI corr note 720863: first read j_1btxip2, read j_1btxip3 with matnr,
* then with NCM and if still nothing is found, then read j_1btxip1.
    read table lt_txip2 with key matnr = rectipo75-matnr_long.
    if sy-subrc = 0.
      rectipo75-ipirate = lt_txip2-rate * 100.
    else.
      loop at lt_txip3 where value = rectipo75-matnr_long.
        rectipo75-ipirate = lt_txip3-rate * 100.
        exit.
      endloop.
      loop at lt_txip3 where value = rectipo75-nbm_orig.
        rectipo75-ipirate = lt_txip3-rate * 100.
        exit.
      endloop.
      if rectipo75-ipirate is initial.
        read table lt_txip1 with key nbmcode = rectipo75-nbm_orig.
        if sy-subrc = 0.
          rectipo75-ipirate = lt_txip1-rate * 100.
        endif.
      endif.
    endif.                                        " EOI corr note 720863

* fill ICMS taxrate with values from j_1btxic1/j_1btxic2
*    read table lt_txic1 index 1.                 " BOD corr note 720863
*    if sy-subrc = 0.
*      rectipo75-icmsrate = lt_txic1-rate * 100.
*    else.
*      read table lt_txic2 with key matnr = rectipo75-matnr_long.
*      if sy-subrc = 0.
*        rectipo75-icmsrate = lt_txic2-rate * 100.
*      else.
*        loop at lt_txic3.
*          if lt_txic3-value = rectipo75-matnr_long.
*            rectipo75-icmsrate = lt_txic3-rate * 100.
*            exit.
*          endif.
*        endloop.
*      endif.
*    endif.                                       " EOD corr note 720863
* BOI corr note 720863: first read j_1btxic2, if nothing found, read
* j_1btxic3, if nothing is found, then read j_1btxic1
    read table lt_txic2 with key matnr = rectipo75-matnr_long.
    if sy-subrc = 0.
      rectipo75-icmsrate = lt_txic2-rate * 100.
    else.
      loop at lt_txic3.
        if lt_txic3-value = rectipo75-matnr_long.
          rectipo75-icmsrate = lt_txic3-rate * 100.
          exit.
        endif.
      endloop.
      if rectipo75-icmsrate is initial.
        read table lt_txic1 index 1.
        if sy-subrc = 0.
          rectipo75-icmsrate = lt_txic1-rate * 100.
        endif.
      endif.
    endif.                                        " EOI corr note 720863

* fill ICMS base (percentage) with values from j_1btxic2
    read table lt_txic2 with key matnr = rectipo75-matnr_long.
    if sy-subrc = 0.
      rectipo75-icmsbasred = ( 100 - lt_txic2-base ) * 100. " nt. 720863
    else.
      loop at lt_txic3.
        if lt_txic3-value = rectipo75-matnr_long.
          check not lt_txic3-base is initial.
          rectipo75-icmsbasred = ( 100 - lt_txic3-base ) * 100." 720863
          exit.
        endif.
      endloop.
    endif.

* fill ICMS ST base
    loop at lt_txst1 where matnr = rectipo75-matnr_long.
      check not lt_txst1-sur_type is initial.
      rectipo75-subtrib = lt_txst1-price * 100.
      exit.
    endloop.                                           " EOI note 608129

* for record 75: exclude cancelled documents, if they come from
* different company
*  LOOP AT rectipo75.                             " DEL note 608129
*    IF rectipo75-direct = '1' AND NOT rectipo75-entrad = 'X'. "nt608129
    if rectipo75-cancel = 'X' and      " note 608129
       rectipo75-direct = '1' and      " note 608129
       rectipo75-entrad is initial.    " note 608129
      delete rectipo75.
    endif.
    modify rectipo75.                  " note 608129
  endloop.

  loop at rectipo75.
    count75 = count75 + 1.
    clear rectipo75-sort_pur_sal.
    perform output using rectipo75.
  endloop.
endform.                                                    " OUTPUT75

*&---------------------------------------------------------------------*
*&      Form  DETERMINE_PARTNER_STATE
*&---------------------------------------------------------------------*
*        determination of the state of the 'bill to party'
*----------------------------------------------------------------------*
form determine_partner_state.

  data: wa_parnad like parnad.

  select single * from j_1bnfnad into corresponding fields
                      of *j_1bnfdoc where docnum = j_1bnfdoc-docnum
                                       and parvw  = 'RE'.

  if sy-subrc = 0.

    call function 'J_1B_NF_PARTNER_READ'
      exporting
        partner_type     = *j_1bnfdoc-partyp
        partner_id       = *j_1bnfdoc-parid
        doc_number       = *j_1bnfdoc-docnum
        partner_function = *j_1bnfdoc-parvw
      importing
        parnad           = wa_parnad
      exceptions
        others           = 04.

    if sy-subrc = 0.
      parnad-regio = wa_parnad-regio.
      parnad-cgc   = wa_parnad-cgc.
      parnad-stains = wa_parnad-stains.
    endif.

  endif.

endform.                               " DETERMINE_PARTNER_STATE

*&---------------------------------------------------------------------*
*&      Form  output_add_rec54
*&---------------------------------------------------------------------*
*  routine required for record 54: filling itmnum '991', '992', '999'
*----------------------------------------------------------------------*
form output_add_rec54 using p_itmnum p_icmsbase.
  clear: rectipo54-matnr,
*         rectipo54-linnftot,                              " note 762594
         rectipo54-linnfnet,                               " note 762594
         rectipo54-icmsbase,
         rectipo54-icstbase,
         rectipo54-stxtaxval.
  clear: rectipo54-st_code,                                " note 838208
         rectipo54-rate,                                   " note 838208
         rectipo54-nfqty.                                  " note 838208
  rectipo54-itmnum   = p_itmnum.
  rectipo54-discount = p_icmsbase.
*  count54 = count54 + 1.                                  " note 838208
  perform output using rectipo54.
endform.                               " output_add_rec54

*&--------------------------------------------------- BOD note 660429 -*
*&      Form  condense_matnr
*&---------------------------------------------------------------------*
* material number in R/3 has length 18 => reported with length 14
*----------------------------------------------------------------------*
*FORM condense_matnr USING     p_original_matnr  p_condensed_matnr.
**  DATA: help TYPE i.                                  " note 608129
*  p_condensed_matnr = p_original_matnr.                " note 608129
*  SHIFT p_condensed_matnr LEFT DELETING LEADING '0'.   " note 608129
**  help = strlen( p_condensed_matnr ).                 " note 608129
**  IF help > 14 AND p_condensed_matnr(1) CA '0 '.      " note 608129
**    help = help - 14.                                 " note 608129
**    DO help TIMES.                                    " note 608129
**      SHIFT p_condensed_matnr.                        " note 608129
**    ENDDO.                                            " note 608129
**  ENDIF.                                              " note 608129
*
*ENDFORM.                               " condense_matnr EOD note 660429

*&---------------------------------------------------------------------*
*&      Form  condense_nbm
*&---------------------------------------------------------------------*
*  NBM in R/3 has len. 10 with format XXXX.XX.XX => reported with len. 8
*----------------------------------------------------------------------*
form condense_nbm using    p_original_nbm  p_condensed_nbm.
  p_condensed_nbm = p_original_nbm.
  replace '.' with ' ' into p_condensed_nbm.
  replace '.' with ' ' into p_condensed_nbm.
  condense p_condensed_nbm no-gaps.
  shift p_condensed_nbm left deleting leading '0'.

endform.                               " condense_nbm

*&---------------------------------------------------------------------*
*&      Form  read_reference_nf
*&---------------------------------------------------------------------*
*  routine required for record 71: reading reference NF for cohecimento
*----------------------------------------------------------------------*
form read_reference_nf.

  data:  wk_indoc        like j_1bindoc,
         wk_lin          like j_1bnflin occurs 0 with header line,
         wk_stx          like j_1bnfstx occurs 0 with header line.

  select single * from j_1bnfdoc where docnum = it_records-docref.

  if sy-subrc = 0.

* determine data of partner for reference Nota Fiscal
    call function 'J_1B_NF_PARTNER_READ'
      exporting
        partner_type     = j_1bnfdoc-partyp
        partner_id       = j_1bnfdoc-parid
        partner_function = j_1bnfdoc-parvw
        doc_number       = j_1bnfdoc-docnum
      importing
        parnad           = parnad
      exceptions
        others           = 04.
    if sy-subrc = 0.

* if the partner has got no insc. est. (stains) (overseas operations or
* partner not obliged to have a insc. est.), the literal 'ISENTO' is
* assigned to this field
      if parnad-stains is initial.
        move const_out-nostains to parnad-stains.
      endif.

* if the partner is not brazilian, the literal 'EX' is assigned to regio
      if parnad-land1 ne address-country.
        move const_out-noregio to parnad-regio.
        clear parnad-cgc.
        clear parnad-cpf.
        move const_out-nostains to parnad-stains.
      endif.

      move parnad-regio    to rectipo71-regio2.
      move parnad-stains   to rectipo71-stains2.

* in case of natural person, CGC is empty, CPF must be reported
      if parnad-stkzn = 'X'.
        rectipo71-cgc2(3) = '000'.  "CPF has length 11 -> 3 leading zeros
        rectipo71-cgc+3(11) = parnad-cpf.
      else.
        rectipo71-cgc2 = parnad-cgc.
      endif.

* delete ., -, / from regional tax code
      if not parnad-stains is initial and
         not parnad-stains eq const_out-nostains.
        clear it_records-stains.
        perform format_stains using parnad-stains
                           changing rectipo71-stains2.
      endif.

    endif.

    rectipo71-issuedat          = j_1bnfdoc-pstdat.
    rectipo71-model_relnf       = j_1bnfdoc-model.
    rectipo71-nfnum_relnf       = j_1bnfdoc-nfnum.
    perform fill_nfserie using rectipo71-series_relnf
                               j_1bnfdoc-series
                               j_1bnfdoc-subser.

* fill NF total value
    select * from  j_1bnflin where docnum = j_1bnfdoc-docnum.

      select * from *j_1bnflin into wk_lin
                              where docnum = j_1bnfdoc-docnum
                                and itmnum = j_1bnflin-itmnum.
        append wk_lin.
      endselect.

      select * from j_1bnfstx into wk_stx
                              where docnum = j_1bnfdoc-docnum
                                and itmnum = j_1bnflin-itmnum.
        append wk_stx.
      endselect.

    endselect.

    call function 'J_1B_NF_VALUE_DETERMINATION'
      exporting
        nf_header   = j_1bnfdoc
      importing
        ext_header  = wk_indoc
      tables
        nf_item     = wk_lin
        nf_item_tax = wk_stx
      exceptions
        others      = 1.
    if sy-subrc = 0.
      rectipo71-linnftot = wk_indoc-nftot  * 100.
    endif.

  endif.
endform.                               " read_reference_nf

*&---------------------------------------------------------------------*
*&      Form  fill_nfserie
*&---------------------------------------------------------------------*
*   special logic for handling NF serie/subserie
*----------------------------------------------------------------------*
form fill_nfserie using p_arqmagserie p_serie p_subser.
  if p_serie(1) ca 'BCEU'.
    p_arqmagserie(1) = p_serie(1).
    if p_subser(1) ca '0123456789'.
      p_arqmagserie+1(2)   = p_subser.
    else.
      p_arqmagserie+1(1)   = p_subser.
    endif.
  elseif p_serie(1) ca '0123456789'.
    p_arqmagserie = p_serie.
  else.
    p_arqmagserie(1)   = p_serie.
    p_arqmagserie+1(1) = p_subser.
  endif.
  if p_serie(1) na '0123456789'.
    p_arqmagserie = ' '.
  endif.
endform.                               " fill_nfserie

*&---------------------------------------------------------------------*
*&      Form  get_inventory
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form get_inventory.
  clear mbew.
* check if material value is updated
  call function 'T134M_SINGLE_READ'
    exporting
      t134m_bwkey = plants-low                           "#EC DOM_EQUAL
      t134m_mtart = material-mtart
    importing
      wt134m      = is_t134m
    exceptions
      not_found   = 1
      others      = 2.

  if sy-subrc = 0.
    check is_t134m-wertu = 'X'.

    call function 'MBEW_SINGLE_READ'
      exporting
        matnr             = material-matnr
        bwkey             = plants-low                   "#EC DOM_EQUAL
        bwtar             = space
      importing
        wmbew             = mbew
      exceptions
        lock_on_mbew      = 1
        lock_system_error = 2
        wrong_call        = 3
        not_found         = 4
        others            = 5.
    if sy-subrc = 0.                   " Material exists in plant
      perform read_mbew_history.       " MBEWH only exist in rel. >40B

* Consulting note 390233 (Reporting historic stocks in Release 4.0)
*      PERFORM GET_MSEG_DATA.

* sum up the stock in different plants of same branch
      summe-tot_quant = summe-tot_quant + mbew-lbkum.
      summe-tot_cost  = summe-tot_cost  + mbew-salk3.

      perform get_consignment.

      loop at it_consig_summe.
        summe-deb_quant = summe-deb_quant + it_consig_summe-deb_quant.
        summe-cre_quant = summe-cre_quant + it_consig_summe-cre_quant.
      endloop.

    endif.                             "MBEW sy-subrc = 0
  endif.                               "T134M sy-subrc = 0

endform.                               " get_inventory

*&---------------------------------------------------------------------*
*&      Form  get_month_end_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form get_month_end_date.
  data: i_lfmon type marv-lfmon,
        i_lfgja type marv-lfgja,
        help_date like sy-datum.

  i_lfmon =     rep_date_high+4(2) + 1."next period

  if i_lfmon > 12.                     "check if next year
    i_lfmon = 1.
    i_lfgja =  rep_date_high(4) + 1.
  else.
    i_lfgja =  rep_date_high(4).
  endif.

  help_date(4)           = i_lfgja.
  help_date+4(2)         = i_lfmon.
  help_date+6(2)         = '01'.       "first day of next period
  rectipo74-invdat       =  help_date - 1.    "last day of period

endform.                               " get_month_end_date

*&---------------------------------------------------------------------*
*&      Form  READ_MBEW_HISTORY
*&---------------------------------------------------------------------*
form read_mbew_history.
* read historic mbew data
  call function 'J_1B_READ_MBEW_MBEWH'
    exporting
      i_mbew       = mbew
    importing
      e_mbew       = mbew
      e_mbewh      = mbewh
    exceptions
      no_selection = 1
      others       = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                               " read_mbew_history

*&---------------------------------------------------------------------*
*&      Form  PREPARE_IT_MSKU
*&---------------------------------------------------------------------*
*       fill internal table it_msku for given material for all plants
*----------------------------------------------------------------------*
form prepare_it_msku.
  refresh it_msku.
  select * from msku into corresponding fields of table it_msku
   where matnr = material-matnr
   and werks in plants
   and sobkz = 'W'.
endform.                               " PREPARE_IT_MSKU

*&---------------------------------------------------------------------*
*&      Form  PREPARE_IT_MSLB
*&---------------------------------------------------------------------*
*       fill internal table it_mslb for given material for all plants
*----------------------------------------------------------------------*
form prepare_it_mslb.
  refresh it_mslb.
  select * from mslb into corresponding fields of table it_mslb
   where matnr = material-matnr
   and werks in plants
   and sobkz = 'O'.
endform.                               " PREPARE_IT_MSLB

*&---------------------------------------------------------------------*
*&      Form  fill_plants
*----------------------------------------------------------------------*
form fill_plants.

  data: begin of lt_plants occurs 0,
          werks like t001w-werks,
        end of  lt_plants.

  select t001w~werks into table lt_plants
          from t001w inner join t001k
               on t001k~bwkey = t001w~werks
              where t001w~j_1bbranch = j5_brnch
              and   t001k~bukrs      = j5_bukrs.

  clear plants[].
  plants-option = 'EQ'.
  plants-sign = 'I'.

  loop at lt_plants.
    plants-low = lt_plants-werks.
    append plants.
  endloop.

* continue only if plants for chosen branch exist
  describe table plants lines lines.
  if not lines > 0.
    message w074(8b) with j5_brnch.
  endif.

endform.                               " fill_plants

*&---------------------------------------------------------------------*
*&      Form  report_summe_rec74
*&---------------------------------------------------------------------*
*  Report the valuations of all selected periods for a given material  *
*  if it is no consumption material                                    *
*----------------------------------------------------------------------*
form report_summe_rec74.
* ---------------------------------------------------------------------*
* mat. number in R/3 has length 18 => reported with leng. 14; nt. 608129
*  MOVE material-matnr   TO rectipo74-matnr.               " note 608129
*  PERFORM condense_matnr USING material-matnr matnr_short." note 608129
*  MOVE matnr_short   TO rectipo74-matnr.                  " note 608129
* BADI for processing material number                  " BOI note 660429
  call method if_ex_badi_j_1blfa1->process_material_number
    exporting
      is_matnr = material-matnr
    importing
      ev_matnr = matnr_short.

  if not matnr_short is initial.
    rectipo74-matnr = matnr_short.
  else.
    rectipo74-matnr = material-matnr.
  endif.                                             " EOI note 660429

  perform get_month_end_date.
  rectipo74-location     = '1'.
*  rectipo74-cgc          = cgc_number.                    " note 608129
*  rectipo74-stains       = branch_data-state_insc.        " note 608129
*  rectipo74-regio        = address-region.                " note 608129
  if not summe-tot_quant is initial.

    summe-own_quant = summe-tot_quant - summe-deb_quant
                          - summe-cre_quant.
    rectipo74-mat_quantity = summe-own_quant * 1000.
    rectipo74-total_cost   = summe-own_quant * 100 / summe-tot_quant
                           * summe-tot_cost.
  else.                 " report stocks with inventory 0
    clear: rectipo74-total_cost .
  endif.                "tot_quant not initial
  append rectipo74.

* report existing debitor consignments

  loop at it_consig_summe.
    if not it_consig_summe-deb_quant is initial and
                                   not summe-tot_quant is initial.
      call function 'KNA1_SINGLE_READ'
        exporting
          kna1_kunnr = it_consig_summe-parid
        importing
          wkna1      = ls_kna1
        exceptions
          not_found  = 1
          others     = 2.
      if sy-subrc <> 0.
      endif.
      rectipo74-total_cost   = it_consig_summe-deb_quant
                               * 100 / summe-tot_quant * summe-tot_cost.
      rectipo74-cgc          = ls_kna1-stcd1.
      rectipo74-stains       = ls_kna1-stcd3.
      rectipo74-regio        = ls_kna1-regio.
      rectipo74-location     = '2'.
      rectipo74-mat_quantity  = it_consig_summe-deb_quant * 1000.
      append rectipo74.

    elseif not it_consig_summe-cre_quant is initial and
                                   not summe-tot_quant is initial.
      call function 'LFA1_SINGLE_READ'
        exporting
          lfa1_lifnr = it_consig_summe-parid
        importing
          wlfa1      = ls_lfa1
        exceptions
          not_found  = 1
          others     = 2.

      if sy-subrc <> 0.
      endif.
      rectipo74-total_cost   = it_consig_summe-cre_quant
                               * 100 / summe-tot_quant * summe-tot_cost.
      rectipo74-cgc          = ls_lfa1-stcd1.
      replace '-' with ' ' into ls_kna1-stcd3.        " note 637576
      condense ls_kna1-stcd3 no-gaps.                 " note 637576
      rectipo74-stains       = ls_lfa1-stcd3.
      rectipo74-regio        = ls_lfa1-regio.
      rectipo74-location     = '3'.
      rectipo74-mat_quantity = it_consig_summe-cre_quant * 1000.
      append rectipo74.

    endif.

  endloop.

endform.                               " report_summe_rec74

*&-------------------------------------------------- --------------- --*
*&      Form  get_consignment
*&---------------------------------------------------------------------*
*     Get all consignments for given plant and period
*----------------------------------------------------------------------*
form get_consignment.

  data: lv_tabix type sytabix.
*  get all debitor consignments for current plant and period
  loop at it_msku where matnr = material-matnr
                  and   werks = plants-low
                  and   sobkz = 'W'.
    call function 'J_1B_READ_MSKU_MSKUH'
      exporting
        i_msku       = it_msku
      importing
        e_msku       = it_msku
      exceptions
        no_selection = 1
        others       = 2.

    if sy-subrc = 0.
      clear wa_consig_summe.
      read table it_consig_summe into wa_consig_summe
                                with key parid = it_msku-kunnr.
      lv_tabix = sy-tabix.
      wa_consig_summe-parid = it_msku-kunnr.
      if sy-subrc = 0.
        wa_consig_summe-deb_quant =
                       wa_consig_summe-deb_quant + it_msku-kulab.
        modify it_consig_summe from wa_consig_summe index lv_tabix.
      else.
        wa_consig_summe-deb_quant = it_msku-kulab.
        append wa_consig_summe to it_consig_summe.
      endif.
    endif.

  endloop.

*  get all creditor consignments for current plant and period
  loop at it_mslb where matnr = material-matnr
                  and   werks = plants-low
                  and   sobkz = 'O'.
    call function 'J_1B_READ_MSLB_MSLBH'
      exporting
        i_mslb       = it_mslb
      importing
        e_mslb       = it_mslb
      exceptions
        no_selection = 1
        others       = 2.

    if sy-subrc = 0.
      clear wa_consig_summe.
      read table it_consig_summe into wa_consig_summe
                                with key parid = it_mslb-lifnr.
      lv_tabix = sy-tabix.
      wa_consig_summe-parid = it_mslb-lifnr.
      if sy-subrc = 0.
        wa_consig_summe-cre_quant =
                       wa_consig_summe-cre_quant + it_mslb-lblab.
        modify it_consig_summe from wa_consig_summe index lv_tabix.
      else.
        wa_consig_summe-cre_quant = it_mslb-lblab.
        append wa_consig_summe to it_consig_summe.
      endif.
    endif.

  endloop.

endform.                               " get_consignment

*&---------------------------------------------------------------------*
*&      Form  fill_tipo_55
*&---------------------------------------------------------------------*
*  Generating record 55: Simply reading information from J_1BLFA1_REC55
*  (except tipo, CGC, region and state inscr.) and give it out
*----------------------------------------------------------------------*
form fill_tipo_55.
  move: '55'                   to rectipo55-tipo,
        cgc_number             to rectipo55-cgc,      " cgc number
        address-region         to rectipo55-regio.    " region

  select * from j_1blfa1_rec55 where bukrs  =  j5_bukrs
                                 and branch =  j5_brnch
                                 and docdat in j5_pdate.
    move-corresponding j_1blfa1_rec55 to rectipo55.
    rectipo55-value = j_1blfa1_rec55-value * 100.
    count55 = count55 + 1.
    perform output using rectipo55.
  endselect.

endform.                               " fill_tipo_55

*&---------------------------------------------------------------------*
*&      Form  determine_record_number
*&---------------------------------------------------------------------*
*  count the record types created (maximum = 12: record 50, 51, 53, 54,
*  55, 56, 70, 71, 74, 75, 76, 77)
*----------------------------------------------------------------------*
form determine_record_number using p_count p_countrecords.
  if not p_count is initial.
    p_countrecords = p_countrecords + 1.
  endif.
endform.                               " determine_record_number

*&------------------------------------------------ BOI note 637576 ----*
*&      Form  modify_material_table
*&---------------------------------------------------------------------*
*       prepare rectipo75 that each material is unique
*----------------------------------------------------------------------*
form modify_material_table.
  data: numb_materials type i.
  data: wa_rectipo75 like rectipo75.                       " note 608129

  describe table rectipo75 lines numb_materials.
  if numb_materials > 0.
    sort rectipo75
            by matnr sort_pur_sal ascending direct entrad descending.
*    DELETE ADJACENT DUPLICATES FROM rectipo75         " BOI note 608129
*                            COMPARING matnr_long st_code.
    delete adjacent duplicates from rectipo75              " note 720863
                            comparing matnr_long.          " note 720863
*    SORT rectipo75 BY matnr st_code.                      " note 720863
    sort rectipo75 by matnr.                               " note 720863
    loop at rectipo75 into wa_rectipo75.
*      LOOP AT rectipo75 WHERE matnr   = wa_rectipo75-matnr    "nt720863
*                          AND st_code = wa_rectipo75-st_code. "nt720863
      loop at rectipo75 where matnr   = wa_rectipo75-matnr. "nt720863
        wa_rectipo75-counter =  wa_rectipo75-counter + 1.
      endloop.
      modify rectipo75 from wa_rectipo75.
      clear wa_rectipo75.
    endloop.

    sort rectipo75 by matnr ascending counter descending.
    delete adjacent duplicates from rectipo75
                            comparing matnr_long.      " EOI note 608129
  endif.

endform.                    " modify_material_table

*&---------------------------------------------------------------------*
*&      Form  fill_products
*&---------------------------------------------------------------------*
* fill the material table not via NFs but report all materials for the
* company code/plant
*----------------------------------------------------------------------*
form fill_products.

  data: begin of it_product occurs 0,
*---> 30/05/2023 - Migração S4 - JS
          "matnr(18)     type c,
          matnr         type mara-matnr,
*<--- 30/05/2023 - Migração S4 - JS
          werks(4)      type c,
          meins         like mara-meins,
          laeda         type d,
          ersda         type d,
          maktx(40)     type c,
          matkl(9)      type c,
          j_1bnbm(16)   type c,
        end of it_product,
        wa_product like it_product.

  data: lt_product_descr like it_product occurs 0,
        ls_product_descr like it_product.
  data: lt_product_nbm like it_product occurs 0,
        ls_product_nbm like it_product.

  refresh rectipo75.

* 1.step: selecting all material from MARA (procedure via MARC takes
* care to get only materials for the selected plants)
  select b~matnr a~meins a~matkl a~laeda a~ersda
      into corresponding fields of table it_product
      from mara as a inner join marc as b on a~matnr = b~matnr
      where b~werks in plants.

  sort it_product by matnr.
  delete adjacent duplicates from it_product.

* reading the material description from MAKT
  select matnr maktx from makt
      into corresponding fields of table lt_product_descr
      for all entries in it_product
      where spras = sy-langu
        and matnr = it_product-matnr.
  sort lt_product_descr by matnr.

* reading the NBM code from T023
  select matkl j_1bnbm from t023
      into corresponding fields of table lt_product_nbm
      for all entries in it_product
      where matkl = it_product-matkl.
*---> 05/07/2023 - Migração S4 - DL
  SORT lt_product_descr BY matnr.
  SORT lt_product_nbm BY matkl.
*<--- 05/07/2023 - Migração S4 - DL
  loop at it_product.
    rectipo75-tipo       = '75'.
    rectipo75-inidate    = j5_pdate-low.  "initial date
    rectipo75-finaldate  = j5_pdate-high. "final date
    rectipo75-matnr_long = it_product-matnr.
    rectipo75-meins      = it_product-meins.
* material number in R/3 has length 18 => reported with length 14
*    PERFORM condense_matnr USING it_product-matnr matnr_short."nt660429
* BADI for processing material number
    call method if_ex_badi_j_1blfa1->process_material_number"nt660429
      exporting
        is_matnr            = it_product-matnr
      importing
        ev_matnr            = matnr_short.

    if not matnr_short is initial.
      rectipo75-matnr = matnr_short.
    else.
      rectipo75-matnr = it_records-matnr.
    endif.

    read table lt_product_descr into ls_product_descr
      with key matnr = it_product-matnr binary search.
    if sy-subrc is initial.
      rectipo75-decript = ls_product_descr-maktx.
    endif.
    read table lt_product_nbm into ls_product_nbm
      with key matkl = it_product-matkl binary search.
    if sy-subrc is initial.
* NBM in R/3 has len. 10 with format XXXX.XX.XX => reported with len. 8
      perform condense_nbm using ls_product_nbm-j_1bnbm tmp_nbm.
      rectipo75-nbm      = tmp_nbm.
      rectipo75-nbm_orig = ls_product_nbm-j_1bnbm.
    endif.

** filling the st_code                                 " BOD note 720863
*    CALL FUNCTION 'MBEW_SINGLE_READ'
*         EXPORTING
*              matnr             = it_product-matnr
*              bwkey             = plants-low             "#EC DOM_EQUAL
*              bwtar             = space
*         IMPORTING
*              wmbew             = mbew
*         EXCEPTIONS
*              lock_on_mbew      = 1
*              lock_system_error = 2
*              wrong_call        = 3
*              not_found         = 4
*              OTHERS            = 5.
*    IF sy-subrc = 0.                   " Material exists in plant
*      rectipo75-st_code(1) = mbew-mtorg.
*    ENDIF.
*    rectipo75-st_code+1(2) = '00'.                    " EOD note 720863
    append rectipo75.
  endloop.

endform.                    " fill_products            " EOI note 637576

*&-------------------------------------------- BOI corr note 720863 ---*
*&      Form  FILLING_SITUATION
*&---------------------------------------------------------------------*
* normally filling with 'N', cancelled NF = 'S'
* BADI for filling with 'E'/'X' in case of extemporanea
*----------------------------------------------------------------------*
form filling_situation .

  if j_1bnfdoc-cancel <> 'X'.
    move 'N' to it_records-situation.
  else.
    move 'S' to it_records-situation.
  endif.

* BADI for determination of extemporanea case
  call method if_ex_badi_j_1blfa1->determine_extemporanea
    exporting
      is_j_1bnfdoc    = j_1bnfdoc
    importing
      ev_extemporanea = it_records-situation
      ev_extempdate   = it_records-pstdat.

endform.                    " FILLING_SITUATION     EOI corr note 720863
