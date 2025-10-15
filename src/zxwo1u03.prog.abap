*&---------------------------------------------------------------------*
*&  Include           ZXWO1U03
*&---------------------------------------------------------------------*

 data: p_respo           type c,
       p_reserva         type resb-rsnum,
       p_check_upd_fiori type char01,
       p_retirada        type c.
 clear: p_check_upd_fiori.
 if ( sy-cprog eq 'SAPMHTTP' and caufvd_imp-change eq 'X' ). "Implementação validação pelo app Fiori / AOENNING. / 186781
   p_check_upd_fiori = abap_true.
 endif.

 if ( sy-tcode = 'IW32' and sy-ucomm = 'ARCH' ) or ( p_check_upd_fiori eq abap_true ). "Implementação validação pelo app Fiori / AOENNING. / 186781
   types: begin of ty_ebkn,          " Classificação contábil da requisição de compra
            banfn type ebkn-banfn,  " Nº requisição de compra
            bnfpo type ebkn-bnfpo,  " Nº do item da requisição de compra
            aufnr type ebkn-aufnr,  " Nº ordem
          end of ty_ebkn,

          begin of ty_eban,         " Requisições de compras da ordem
            banfn        type eban-banfn,  " Numero da requisição
            bnfpo        type eban-bnfpo,  " Item da requisiçãp
            statu        type eban-statu,  " Status do item
            frgkz        type eban-frgkz,  " Status da aprovação
            matnr        type eban-matnr,  " Numero do material
            arsnr        type eban-arsnr,  " Numero da reserva
            arsps        type eban-arsps,  " Item da reserva
            ebakz        type eban-ebakz,  " Requisição de compra concluída
            loekz        type eban-loekz,  " Código de eliminação no documento de compras
            wepos        type eban-wepos,  " Código de entrada de mercadorias
            ebeln        type eban-ebeln,  " Nº pedido
            ebelp        type eban-ebelp,  " Nº item do pedido
            pstyp        type eban-pstyp,  " Ctg.item no documento compra
            frgst        type eban-frgst,  " Estratégia de liberação na requisição de compra
            menge        type eban-menge,  " Quantidade da requisição de compra
            bsmng        type eban-bsmng,  " Quantidade pedida da ReqC
            ztipo        type c,           " Tipo de requisição ( M - Material, S - Serviço )
            status_coupa type eban-status_coupa,
          end of ty_eban,

          begin of ty_ekpo,         " Itens pedido de compra.
            ebeln  type ekpo-ebeln,  " Nº do documento de compras
            ebelp  type ekpo-ebelp,  " Nº item do documento de compra
            loekz  type ekpo-loekz,  " Código de eliminação
            elikz  type ekpo-elikz,  " Código de remessa final
            pstyp  type ekpo-pstyp,  " Ctg.item no documento compra
            packno type ekpo-packno, " Nº pacote
          end of ty_ekpo,

          begin of ty_esll,          " Linhas do pacote de serviços
            packno     type esll-packno,     " Nº pacote
            sub_packno type esll-sub_packno, " Nº do subpacote
            menge      type esll-menge,      " Qtd.com símbolo +/-
            act_menge  type esll-act_menge,  " Pedido: qtd.registrada
          end of ty_esll,

          begin of ty_req_pend,
            banfn type eban-banfn,  " Numero da requisição
          end of ty_req_pend,

          begin of ty_ped_pend,
            ebeln type eban-ebeln,  " Nº pedido
          end of ty_ped_pend.

   data: t_eban      type table of ty_eban,
         t_ebkn      type table of ty_ebkn,
         t_esll      type table of ty_esll,
         t_esll_sub  type table of ty_esll,
         t_req_pend  type table of ty_req_pend with header line,
         t_ped_pend  type table of ty_ped_pend with header line,
         w_eban      type ty_eban,
         w_ebkn      type ty_ebkn,
         w_ekpo      type ty_ekpo,
         w_esll      type ty_esll,
         w_esll_sub  type ty_esll,
         l_resul     type eket-menge,
         l_len       type i,
         l_msg(100)  type c,
         l_banfn(82) type c,
         l_ebeln(82) type c.

   clear: t_req_pend, t_ped_pend.
   refresh: t_req_pend[], t_ped_pend[].

   "Verificar itens de reservas pendentes para ordem.
   select * from resb into table @data(t_resb) where aufnr eq @caufvd_imp-aufnr.
   if t_resb is not initial.
     loop at t_resb assigning field-symbol(<ws_resb>).
       if p_retirada is initial.
         if <ws_resb>-enmng ne <ws_resb>-bdmng.
           p_retirada = abap_true.
           p_reserva = <ws_resb>-rsnum.
         endif.
       endif.
     endloop.


     if p_retirada is not initial and p_check_upd_fiori is initial.
       call function 'POPUP_TO_CONFIRM'
         exporting        "TITLEBAR = 'Confirmar'
           text_question         = 'Exitem itens de reserva pendente, deseja seguir com encerramento da ordem?'
           text_button_1         = 'Ok'
           text_button_2         = 'Não'
           display_cancel_button = ' '
         importing
           answer                = p_respo.

       if p_respo = 2.
         message e398(00) with |Verificar itens da reserva { p_reserva } pendente.|.
       endif.
     endif.
   endif.
   free: t_resb.
   clear: p_retirada, p_respo, p_reserva.

* Verifica se existem requisições vinculadas a ordem.
   select banfn bnfpo aufnr into table t_ebkn
     from ebkn
     where aufnr = caufvd_imp-aufnr.

   "Se não existir requisição vinculada a ordem - SAIR.
   if sy-subrc ne 0.
     exit.
   endif.

   break fabap04.
* Verificar se existe requisição aprovada ou item eliminado.
   select banfn bnfpo statu frgkz matnr arsnr arsps ebakz loekz wepos
          ebeln ebelp pstyp frgst menge bsmng status_coupa
     into corresponding fields of table t_eban
     from eban
     for all entries in t_ebkn
     where banfn eq t_ebkn-banfn
     and   bnfpo eq t_ebkn-bnfpo.

   if t_eban is initial.
     exit.     " Não existe requisição .
   endif.

   loop at t_eban into w_eban.

     if w_eban-frgst is initial.
       continue.
     endif.

*-US 154383-06-11-2024-#154383-RJF-Inicio
* se requisição aprovada e item não eliminado
*     IF w_eban-frgkz EQ 'X'.              " Requisição aprovada.
*       CONTINUE.
*     ELSEIF w_eban-loekz IS NOT INITIAL.  " Item não eliminado.
*       CONTINUE.
*     ENDIF.

*     IF w_eban-frgkz NE 'X'.              " Requisição aprovada.
*       CONTINUE.
*     ENDIF.

*-US 154383-06-11-2024-#154383-RJF-Fim

     " Verifica status da requisição no coupa
     if w_eban-status_coupa is not initial.
       t_req_pend-banfn = w_eban-banfn.
       append t_req_pend .
       continue.
     endif.

* se Requisição pendente - EBAN-STATU – Status de processamento.
* Observar as seguintes regras:
* Para “N” – Não processado, ou “A” Sol. Cotação criada
* bloquear o encerramento técnico.
     if w_eban-statu = 'N' or         " Não processada.
        w_eban-statu = 'A'.           " Sol. cotação
       t_req_pend-banfn = w_eban-banfn.
       append t_req_pend .
       continue.
     endif.

* Para EBAN-STATU = “B” – Pedido criado verificar pedido.
     if w_eban-statu ne 'B'.          " Pedido não criado
       continue.
     endif.

* Se qtde solicitada ReqC menos Qtde pedida na ReqC diferente de zero
* enviar msg de Requisição Pendente.
     l_resul = w_eban-menge - w_eban-bsmng.

     if l_resul ne 0.
       t_req_pend-banfn = w_eban-banfn.
       append t_req_pend .
       clear l_resul.
       continue.
     endif.

* Pedido criado:
* verificar na tabela EKPO as seguintes condições:
* 1 - EKPO-LOEKZ – Código de eliminação = branco
* 2 - EKET-MENGE – Qtde. da divisão – EKET-WEMNG – Fornecido  # de 0, envia msg.
     select single ebeln ebelp loekz elikz pstyp packno into w_ekpo
       from ekpo
       where ebeln eq w_eban-ebeln
       and   ebelp eq w_eban-ebelp.

     if w_ekpo-loekz is not initial.
       continue.
     endif.

     if w_ekpo-pstyp ne '9'.

*    Se ekpo-elikz for branco.
*    envia mensagem de pedido pendente.
       if w_ekpo-elikz is initial.
         t_ped_pend-ebeln = w_ekpo-ebeln.
         append t_ped_pend .
         continue.
       endif.
     else.
       select packno sub_packno menge act_menge
         from esll
         into table t_esll
         where packno = w_ekpo-packno.
       if sy-subrc ne 0.
         continue.
       endif.

       loop at t_esll into w_esll.
         select packno sub_packno menge act_menge
         from esll
         into table t_esll_sub
         where packno = w_esll-sub_packno.
         if sy-subrc ne 0.
           continue.
         endif.
         loop at t_esll_sub into w_esll_sub.
           l_resul = w_esll_sub-menge - w_esll_sub-act_menge.
           if l_resul ne 0.
             t_ped_pend-ebeln = w_ekpo-ebeln.
             append t_ped_pend .
           endif.
         endloop.
       endloop.
     endif.
   endloop.

*  select single ebeln ebelp etenr menge wemng
*    from eket
*    into w_eket
*    where ebeln  = w_ekpo-ebeln
*    and   ebelp  = w_ekpo-ebelp.
* Se EKET-MENGE – EKET-WEMNG é diferente de zero
* envia mensagem de pedido pendente.
*    l_resul = w_eket-menge - w_eket-wemng.
*    if l_resul ne 0.
*      t_ped_pend-ebeln = w_eket-ebeln.
*      append t_ped_pend .
*      continue.
*    endif.
*  endif.
*endloop.

   if t_req_pend is not initial.
     sort t_req_pend by banfn.
     delete adjacent duplicates from t_req_pend comparing all fields.
     loop at t_req_pend.
       concatenate t_req_pend-banfn ',*' l_banfn into l_banfn.
     endloop.
     l_len = strlen( l_banfn ).
     l_len = l_len - 2.
     translate l_banfn using '* '.
     translate l_banfn+l_len using ',.'.
     concatenate '>>> Requisições Pendentes' l_banfn into l_msg
           separated by space.
     message l_msg type 'I'.
   endif.

   if t_ped_pend is not initial.
     sort t_ped_pend by ebeln.
     delete adjacent duplicates from t_ped_pend comparing ebeln.
     loop at t_ped_pend.
       concatenate t_ped_pend-ebeln ',*' l_ebeln into l_ebeln.
     endloop.
     l_len = strlen( l_ebeln ).
     l_len = l_len - 2.
     translate l_ebeln using '* '.
     translate l_ebeln+l_len using ',.'.
     concatenate '>>> Pedido(s) Pendente(s)' l_ebeln into l_msg
               separated by space.
     message l_msg type 'I'.
   endif.

   if t_req_pend is not initial or
      t_ped_pend is not initial.
     message 'Não foi possível encerrar.' type 'E'.
   endif.

 endif.
