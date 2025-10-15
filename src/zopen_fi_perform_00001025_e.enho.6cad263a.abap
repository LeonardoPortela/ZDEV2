"Name: \FU:OPEN_FI_PERFORM_00001025_E\SE:BEGIN\EI
ENHANCEMENT 0 ZOPEN_FI_PERFORM_00001025_E.
* Preencimento Campo VBEL2 BSEG/BSID Pathelle - 05/07/2011
  data: vl_vbel2     type char20 value '(SAPMF05A)BSEG-VBEL2' ,
        vl_ebeln     type char20 value '(SAPMF05A)BSEG-EBELN' ,
        vl_hzuon     type char21 value '(SAPMF05A)XBSEG-HZUON',
        vl_hzuon2    type char20 value '(SAPMF05A)BSEG-HZUON',
        vl_ZUONR     type char20 value '(SAPMF05A)BSEG-ZUONR',
        vl_budat     type char20 value '(SAPMF05A)BKPF-BUDAT',
        vl_waers     type char20 value '(SAPMF05A)BKPF-WAERS',
        vl_bukrs     type char20 value '(SAPMF05A)BKPF-BUKRS',
        vl_budat2    type bkpf-budat,
        st_bseg      type bseg                                ,
        st_zfit0026  TYPE zfit0026,
        vl_blart     type char20 value '(SAPMF05A)BKPF-BLART' , " Alteração conforme solicitação
        vl_blart2    type bkpf-blart,
        vl_blart3    type bkpf-blart,
        vl_blart_vc  type bkpf-blart,
        v1_vbeln     type vbak-vbeln                          , " Adicionado conforme solicitação
        v1_ebeln     type ekko-ebeln,
        v1_ebelp     type ekpo-ebelp,
        vl_bschl     type char21 value '(SAPMF05A)RF05A-NEWBS',
        T_NEW_TABLE  TYPE REF TO DATA,
        T_NEW_line   TYPE REF TO DATA,
        CVBRP        TYPE VBRPVB.

  field-symbols: <VBEL2>    type bseg-vbel2,
                 <EBELN>    type bseg-ebeln,
                 <EBELP>    type bseg-ebelp,
                 <HZUON>    type bseg-hzuon,
                 <HZUON2>   type bseg-hzuon,
                 <ZUONR>    type bseg-ZUONR,
                 <BLART>    type bkpf-blart, " Alteração conforme solicitação
                 <BUDAT>    type bkpf-budat, "
                 <WAERS>    type bkpf-waers, "
                 <BUKRS>    type bkpf-bukrs, "
                 <BSCHL>    type rf05a-newbs,
                 <XHZUON>   type any,
                 <FS_TABLE> TYPE ANY TABLE,
                 <FS_LINE>  type any,
                 <FS_FIELD> type bseg-hzuon,
                 <FS_FIELD2> type bseg-zuonr,
                 <vbrk>     TYPE vbrk,
                 <vbrP>     TYPE vbrP.

  "CS2016001175
  if 'FB05_FB1K_FB1D_FBZ1_FBZ2' cs sy-tcode.
      assign: (vl_budat) to <BUDAT>.
      if <BUDAT> is ASSIGNED.
         clear: vl_blart2, vl_blart3, vl_blart_vc.
         LOOP AT T_AUSZ3.
            if sy-tabix  = 1.
               SELECT SINGLE budat blart into ( vl_budat2, vl_blart2 )
               from bkpf
               where bukrs = T_AUSZ3-bukrs
               and   belnr = T_AUSZ3-belnr
               and   gjahr = T_AUSZ3-gjahr.
               if vl_blart2 = 'VC'.
                  vl_blart_vc = vl_blart2.
               endif.
            elseif ( vl_blart2 eq vl_blart3 ) or vl_blart3 is INITIAL.
               SELECT SINGLE budat blart into ( vl_budat2, vl_blart3 )
               from bkpf
               where bukrs = T_AUSZ3-bukrs
               and   belnr = T_AUSZ3-belnr
               and   gjahr = T_AUSZ3-gjahr.
               if vl_blart3 = 'VC'.
                  vl_blart_vc = vl_blart3.
               endif.
            else.
               SELECT SINGLE budat blart into ( vl_budat2, vl_blart_vc )
               from bkpf
               where bukrs = T_AUSZ3-bukrs
               and   belnr = T_AUSZ3-belnr
               and   gjahr = T_AUSZ3-gjahr
               and   blart = 'VC'.
            endif.
            "
            IF vl_budat2 gt <BUDAT>.
               if sy-langu = 'E'.
                  MESSAGE e899 WITH 'It was chosen for compensation, documents with '
                                    'entry date after the compensation date '.
               elseif sy-langu = 'S'.
                  MESSAGE e899 WITH 'Fueran seleccionados para compensación documentos'
                                    'con fecha lanzamiento mayor que la fecha de compensación'.
               else.
                  MESSAGE e899 WITH 'Foram selecionados para compensação documentos'
                                     'com data de lançamento maior que a data de compensação'.
               endif.
              EXIT.
            ENDIF.
         ENDLOOP.

         assign: (vl_blart) to <BLART>.
         if <BLART> is  ASSIGNED. "
            if vl_blart3 is not INITIAL and  <BLART> ne 'DZ'.
               if vl_blart2 = vl_blart3.
                  vl_blart3 =  <BLART>.
                  if vl_blart2 ne  vl_blart3.
                     assign: (vl_waers) to <WAERS>.
                     assign: (vl_bukrs) to <BUKRS>.
                     if <WAERS> is ASSIGNED and <BUKRS> is ASSIGNED.
                         if   sy-tcode = 'FB05' AND <WAERS> ne 'BRL' and not ( '0100_0101_0200_0201'  cs <BUKRS> ).
                          if sy-langu = 'E'.
                             MESSAGE e899 WITH 'Change the document type to' vl_blart2.
                          ELSEIF sy-langu = 'S'.
                             MESSAGE e899 WITH 'Cambiar el tipo de documento de compensación por' vl_blart2.
                          else.
                             MESSAGE e899 WITH 'Mudar o Tipo de documento de Compensação para ' vl_blart2.
                          endif.
                          EXIT.
                         endif.
                     endif.
                  endif.
               ELSEIF vl_blart_vc = 'VC'.
                  if sy-langu = 'E'.
                     MESSAGE e899 WITH 'It was chosen to compensation The document "VC"'
                                       'should be deleted.'.
                  ELSEIF sy-langu = 'S'.
                       MESSAGE e899 WITH 'Fueron seleccionados para la limpieza de tipo de'
                                         'documento VC que necesita ser eliminado'.
                  else.
                    MESSAGE e899 WITH 'Foram selecionados para compensação '
                                       'Documento tipo VC que precisam ser eliminados'.
                  endif.
                  EXIT.
               endif.
            endif.
         ENDIF.
      endif.
  endif.

** =============Inicio US9516 Miro para notas complementar sem bloqueio / Anderson Oenning - 04/10/2022
  IF sy-tcode eq 'ZMM0079'.
  LOOP at T_BSEG ASSIGNING FIELD-SYMBOL(<LS_BSEG>).
    IF <LS_BSEG>-ZLSPR EQ 'R'.
      <LS_BSEG>-ZLSPR = SPACE. "Criar documento sem bloqueio de pagamento.
    ENDIF.
  ENDLOOP.
  ENDIF.
** =============Fim US9516 Miro para notas complementar sem bloqueio / Anderson Oenning - 04/10/2022

  check (  ( sy-tcode eq   'FB05' ) or ( sy-tcode eq   'FBA1' ) or (   sy-tcode eq 'FBA2' ) or ( sy-tcode eq 'FB1D' ) )  . " Add verificação sy-tcode eq 'FBA2'

  "Geração pela ZFIS26
  IF T_BKPF-AWKEY IS NOT INITIAL.
    SELECT SINGLE *
      FROM zfit0026 INTO st_zfit0026
     WHERE OBJ_KEY  EQ T_BKPF-AWKEY.

    IF sy-subrc = 0.
      LOOP at T_BSEG.
        CHECK T_BSEG-ZUONR is not INITIAL.
        CHECK ( ( T_BSEG-BSCHL = '19' ) OR
                ( T_BSEG-BSCHL = '09' ) OR
                ( T_BSEG-BSCHL = '01' ) OR
                ( T_BSEG-BSCHL = '11' ) ).

        v1_vbeln = T_BSEG-ZUONR(10).

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input         = v1_vbeln
          IMPORTING
            OUTPUT        = v1_vbeln.

        SELECT SINGLE vbeln
          FROM vbak INTO v1_vbeln
         WHERE vbeln EQ v1_vbeln.

        CHECK sy-subrc = 0.

        T_BSEG-vbel2 = v1_vbeln.
        T_BSEG-posn2 = 10.

        modify T_BSEG.
      ENDLOOP.
    ENDIF.
  ENDIF.


  " Adicionado conforme solicitação
  if <BLART> is not ASSIGNED.
     assign: (vl_blart) to <BLART>.
  endif.

  if <BLART> is not ASSIGNED.
     exit.
  endif.

 if <BLART> eq  'DZ'
  or <BLART> eq  'AB'.
     assign: (vl_hzuon2) to <hzuon2>.

  if <hzuon2> is assigned.
     if not ( <hzuon2>  is initial ).

        st_bseg-vbel2 = <hzuon2>.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = st_bseg-vbel2
        IMPORTING
          OUTPUT = st_bseg-vbel2.

        st_bseg-posn2 = 10.
        if not T_BSEG[] is initial.
          modify T_BSEG from st_bseg
          transporting posn2 vbel2
           where posn2 EQ 0 .
        endif.
     endif.
  endif.
endif.

  assign: (vl_vbel2) to <VBEL2>,
          (vl_ebeln) to <EBELN>,
          (vl_hzuon) to <HZUON>,
          (vl_bschl) to <BSCHL>.


   IF ( SY-TCODE EQ 'FB1D' or sy-tcode eq 'FB05' ).
     ASSIGN: ('(SAPMF05A)XBSEG[]') TO <XHZUON>.
     GET REFERENCE OF <XHZUON> INTO T_NEW_TABLE.
     ASSIGN T_NEW_TABLE->* TO <FS_TABLE>.
     CREATE DATA T_NEW_LINE LIKE LINE OF <FS_TABLE>.
* CRIA UMA FIELD-SYMBOL COMO WORK AREA
  ASSIGN T_NEW_LINE->* TO <FS_LINE>.
     LOOP AT <FS_TABLE> INTO <FS_LINE>.
       CLEAR: st_zfit0026.

       ASSIGN: ('<FS_LINE>-HZUON') TO <FS_FIELD>. "atribuicao rz especial
       ASSIGN: ('<FS_LINE>-BSCHL') TO <BSCHL>.
       assign: ('<FS_FIELD>') TO <HZUON>.

       " se for cliente rz especial
       IF ( ( <BSCHL> = '19' ) OR
            ( <BSCHL> = '01' ) OR
            ( <BSCHL> = '11' ) ).
            If ( <HZUON> is initial )  .
                MESSAGE W899 WITH 'Informar Ordem Venda em atrib. Especial' DISPLAY LIKE 'W'.
            endif.
       ENDIF.

       IF <FS_FIELD> IS ASSIGNED.

         "Ini CS2017000330
         IF ( <FS_FIELD> IS NOT INITIAL ) and
            ( <EBELN>    is assigned    ) and
            ( <HZUON>    is assigned    ) and
            ( ( <BSCHL> = '21' ) OR
              ( <BSCHL> = '31' ) OR
              ( <BSCHL> = '29' ) OR
              ( <BSCHL> = '39' ) ).

             CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
               EXPORTING
                 input  = <HZUON>(10)
               IMPORTING
                 OUTPUT  = v1_ebeln.

             v1_ebelp = '00010'.
             IF STRLEN( <HZUON> ) = 15. "Pedido(10) + Item(5)
               v1_ebelp = <HZUON>+10(5).
             ENDIF.

             SELECT SINGLE ebeln ebelp
               FROM ekpo INTO ( v1_ebeln , v1_ebelp )
              WHERE ebeln EQ v1_ebeln
                AND ebelp EQ v1_ebelp.

            IF  ( sy-subrc = 0 ) and ( <HZUON> is not initial ).
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input         = <HZUON>(10)
                IMPORTING
                  OUTPUT        = <EBELN>.

              st_bseg-ebeln = <EBELN>.
              st_bseg-ebelp = v1_ebelp.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input         = st_bseg-ebelp
                IMPORTING
                  OUTPUT        = st_bseg-ebelp.

              if not T_BSEG[] is initial.
                modify T_BSEG from st_bseg
                  transporting ebeln ebelp
                  where hzuon eq <HZUON>.
              endif.
            ENDIF.

         ENDIF.

         "Fim CS2017000330

         IF ( <FS_FIELD> IS NOT INITIAL ) and
            ( ( <BSCHL> = '19' ) OR
              ( <BSCHL> = '01' ) OR
              ( <BSCHL> = '11' ) ).
             CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
               EXPORTING
                 input  = <HZUON>(10)
              IMPORTING
                OUTPUT  = v1_vbeln.

              SELECT SINGLE vbeln
              FROM   vbak
              INTO   v1_vbeln
              WHERE  vbeln EQ v1_vbeln.
               IF ( sy-subrc IS NOT INITIAL ) .
                MESSAGE e899 WITH 'Nº da Ordem de Venda informado não encontrado.'.
                EXIT.
              ENDIF.

          IF <VBEL2> is assigned AND
             <HZUON> is assigned.
            IF not <HZUON> is initial.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input         = <HZUON>
                IMPORTING
                  OUTPUT        = <VBEL2>.

              st_bseg-vbel2 = <VBEL2>.
              st_bseg-posn2 = 10.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input         = st_bseg-posn2
                IMPORTING
                  OUTPUT        = st_bseg-posn2.

              IF sy-tcode eq 'FB05'.
                  if <BLART> eq  'TA'.
                     st_bseg-zterm = 'C002'.
                     st_bseg-posn2 = '0010'.
                  endif.
              ENDIF.
              if not T_BSEG[] is initial.
                modify T_BSEG from st_bseg
                  transporting vbel2 zterm posn2
                  where hzuon eq <HZUON>.
              endif.
            ENDIF.
          ENDIF.
         ENDIF.
       ENDIF.
     ENDLOOP.
else.

  If ( ( <HZUON> is initial ) and ( sy-tcode ne 'FB1D' ) ). "or ( ( sy-tcode eq 'FB1D' )  and ( <BSCHL> eq '19' ) and ( <HZUON> is initial ) ) .
    assign: (VL_ZUONR) to <ZUONR>.
    IF <ZUONR> IS ASSIGNED
    AND <ZUONR> NE 'PERFORMANCE'
    AND <ZUONR> NE 'ALGODAO'
    AND <ZUONR>(4) NE 'P.I.'.  "CS2022000332-#78223-01.07.2022-JT
      MESSAGE e899 WITH 'Informar o Numero da Ordem de Venda'.
      EXIT.
    ENDIF.
  ELSE.

   CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
     EXPORTING
       input  = <HZUON>(10)
    IMPORTING
      OUTPUT  = v1_vbeln.

    SELECT SINGLE vbeln
    FROM   vbak
    INTO   v1_vbeln
    WHERE  vbeln EQ v1_vbeln.
     IF ( sy-subrc IS NOT INITIAL ) .
      MESSAGE e899 WITH 'Nº da Ordem de Venda informado não encontrado.'.
      EXIT.
    ENDIF.
  ENDIF.


  IF <VBEL2> is assigned AND
     <HZUON> is assigned.
    IF not <HZUON> is initial.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input         = <HZUON>
        IMPORTING
          OUTPUT        = <VBEL2>.
      st_bseg-vbel2 = <VBEL2>.
      if not T_BSEG[] is initial.
        modify T_BSEG from st_bseg
          transporting vbel2
          where vbel2 is initial.
      endif.
    ENDIF.
  ENDIF.
endif.
ENDENHANCEMENT.
