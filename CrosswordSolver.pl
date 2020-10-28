%Gustavo Simoes, al95588
:- [codigo_comum].

%obtem_letras_palavras: 
%INPUT: Lst_Pals (lista de palavras),
%OUTPUT: Letras (lista ordenada das palavras, letra a letra).

%----------------------------------------------

obtem_letras_palavras([],[]).                  
obtem_letras_palavras([L|R], [L1|R1]) :- 
    sort([L|R], [SortedL|SortedR]),             %Ordena-se as listas primeiro, obtendo a sua versao ordenada,
    atom_chars(SortedL, L1),                    
    obtem_letras_palavras(SortedR, R1).         %Realiza-se a mesma operacao para a cauda.

%----------------------------------------------

%espaco_fila:
%INPUT: Fila (linha ou coluna de uma grelha),
%OUTPUT: Esp (espaco de Fila).

espaco_fila(Fila, Esp) :-
    espaco_fila_aux(Fila, 0, [], Esp).          %Chama-se um predicado auxiliar, com um acumulador e um contador do tamanho do espaco.

espaco_fila_aux([], Acc_N, Acc_L, Acc_L) :-     %Quando se chega ao final da fila, verifica-se se o tamanho do espaco e maior que 3,
    Acc_N >= 3.

espaco_fila_aux([Fila_I|Fila_F], Acc_N, Acc_L, Esp) :-

    Fila_I \== #,
    Acc_Mais1 is Acc_N + 1,
    append(Acc_L, [Fila_I], Acc_Novo),
    espaco_fila_aux(Fila_F, Acc_Mais1, Acc_Novo, Esp).

espaco_fila_aux([Fila_I|_], Acc_N, Acc_L, Esp) :-
    Fila_I == #,                                 %Quando se encontra um '#', verifica-se se o tamanho do espaco e maior que 3,
    Acc_N >= 3,
    append(Acc_L, [], Esp).

espaco_fila_aux([Fila_I|Fila_F], _,_, Esp) :-
    Fila_I == #,
    espaco_fila_aux(Fila_F, 0, [], Esp).        %Se o tamanho for menor que 3, da-se 'reset' ao acumulador e ao contador.

%----------------------------------------------

%espacos_fila:
%INPUT: Fila,
%OUTPUT: Espacos (lista de todos os espacos de Fila).

espacos_fila(Fila, Espacos) :-
    bagof(Espaco_I, espaco_fila(Fila, Espaco_I), Espacos), 
    !.
espacos_fila(_, []).

%----------------------------------------------

%espacos_puzzle
%INPUT: Grelha,
%OUTPUT: Espacos (lista de espacos de Grelha).

espacos_puzzle([Fila|R_Grelha], Res) :-
    mat_transposta([Fila|R_Grelha], Grelha_T),
    espacos_puzzle_aux([Fila|R_Grelha], [], Res1),      %obtem-se todos os espacos da grelha, linhas e colunas.
    espacos_puzzle_aux( Grelha_T, [], Res2),            
    append(Res1, Res2, Res).
    
 
espacos_puzzle_aux([Fila|R_Grelha], Aux, Res) :-
    [Fila|R_Grelha] \= [],
    espacos_fila(Fila, Espaco),
    append(Aux, Espaco, N_Acc),
    espacos_puzzle_aux(R_Grelha, N_Acc,Res).

espacos_puzzle_aux([], Acc ,Acc).  

%----------------------------------------------

%espacos_com_posicoes_comuns
%INPUT: Espacos (lista de espacos), Esp (Espaco),
%OUTPUT: Esps_com (lista de espacos com variaveis em comum com Esp).

espacos_com_posicoes_comuns([EspI|EspF], Esp1, EspFinal) :-
    espacos_aux([EspI|EspF], Esp1, [], EspFinal),
    !.

espacos_aux([], _, EspFinal, EspFinal).                         %unificar com o resultado no caso base.

espacos_aux([EspI|EspF], Esp1, EspAcc, EspFinal) :-             %se for igual ao espaco dado, ignorar e passar a frente,
    EspI == Esp1,
    espacos_aux(EspF, Esp1, EspAcc, EspFinal).

espacos_aux([EspI|EspF], Esp1, EspAcc, EspFinal) :-             
    \+ comuns(Esp1,EspI),                                       %se nao partilhar posicoes com o espaco dado, ignorar e passar a frente,
    espacos_aux(EspF, Esp1, EspAcc, EspFinal).

espacos_aux([EspI|EspF], Esp1, EspAcc, EspFinal) :-
    EspI \== Esp1,
    comuns(Esp1, EspI),                                         %se partilhar, adicionar ao acumulador.
    append(EspAcc, [EspI], EspAcc_N),
    espacos_aux(EspF, Esp1, EspAcc_N, EspFinal).

espacos_aux([EspI|[]], Esp1, EspAcc, EspFinal) :-
    espacos_aux(EspI, Esp1, EspAcc, EspFinal).

%PREDICADO AUXILIAR: comum/2 - Verifica se existem elementos comuns entre duas listas, devolvendo true no caso afirmativo.

comum(El, [L|_]) :-
    El == L.
comum(El, [_|L1]) :-
    comum(El, L1).

comuns([L|_], L2) :-
    comum(L, L2).
comuns([_|L1], L2) :-
    comuns(L1, L2).

%----------------------------------------------

%palavra_possivel_esp:
%INPUT: Pal (lista de letras de uma palavra), Esp (espaco), Espacos (lista de espacos), Letras e uma (lista de listas de letras de palavras), 
%Avalia se Pal e uma palavra possivel para o espaco Esp.

palavra_possivel_esp(Pal, Esp, Espacos, Letras) :-
    mesmo_comprimento(Pal, Esp),
    espacos_com_posicoes_comuns(Espacos, Esp, EspComuns),       %Retira se as posicoes comuns relativas a um espaco
    Pal = Esp,                                                  %Unifica-se a palavra ao espaco e verifica-se se ha conflito entre a palavra e as possibilidades para os espacos comuns
    !,
    conflito(EspComuns,Letras).

conflito([EspComunsI|EspComunsF],Letras) :-                     %o predicado percorre todas as letras e espacos comuns, verificando se se podem unificar. Tem valor verdadeiro no caso afirmativo e falso no caso negativo.
    EspComunsF \== [],
    conflito_Espaco(EspComunsI, Letras), 
    conflito(EspComunsF, Letras).

conflito([EspComunsI|EspComunsF],Letras) :-
    EspComunsF == [],
    conflito_Espaco(EspComunsI, Letras).

conflito_Espaco(Espaco, [LetrasI|_]) :-
    unifiable(Espaco, LetrasI,_). 

conflito_Espaco(Espaco, [LetrasI|LetrasF]) :-
    \+ unifiable(Espaco, LetrasI,_),
    conflito_Espaco(Espaco, LetrasF). 

mesmo_comprimento(L1,L2) :-                                     %predicado auxiliar, que verifica se duas listas tem o mesmo comprimento
    length(L1, A),
    length(L2, B),
    B =:= A.


%----------------------------------------------

%palavras_possiveis_esp:
%INPUT: Letras (lista de listas de letras de palavras), Espacos (lista de espacos), Esp (espaco),
%OUTPUT: Pals_Possiveis (lista ordenada de palavras possiveis para o espaco Esp).

palavras_possiveis_esp(Letras, Espacos, Esp, Pals_Possiveis) :-
     possiveis_aux(Letras, Letras, Espacos, Esp, [], Pals_Possiveis),
     !.

possiveis_aux([], _, _, _, N_Acc, N_Acc).                                               %No caso base, unifica o acumulador com o resultado
possiveis_aux([LetrasI|LetrasF], Letras, Espacos, Esp, Acc, Pals_Possiveis) :-      
    \+ palavra_possivel_esp(LetrasI, Esp, Espacos, Letras),
    possiveis_aux(LetrasF, Letras, Espacos, Esp, Acc, Pals_Possiveis).                  %Percorre as letras ate encontrar uma palavra possivel para o espaco, adicionando ao unificador no caso afirmativo.

possiveis_aux([LetrasI|LetrasF], Letras, Espacos, Esp, Acc, Pals_Possiveis) :-
    LetrasF \== [],
    append(Acc, [LetrasI], N_Acc),
    possiveis_aux(LetrasF, Letras, Espacos, Esp, N_Acc, Pals_Possiveis).
    
possiveis_aux([LetrasI|LetrasF], _, _, _, Acc, Pals_Possiveis) :-
    LetrasF == [],
    append(Acc, [LetrasI], Pals_Possiveis).


%----------------------------------------------

%palavras_possiveis:
%INPUT: Letras (lista de listas de letras de palavras), Espacos (lista de espacos),
%OUTPUT: Pals_Possiveis (lista de palavras possiveis).

palavras_possiveis(Letras, Espacos, Pals_Possiveis) :-
    palavras_possiveis_aux(Letras, Espacos, Espacos, [] ,Pals_Possiveis).

palavras_possiveis_aux(_, [], _, Acc, Acc).                                                       %Quando se acaba de percorrer lista dos espacos, unifica o acumulador com o resultado. 
palavras_possiveis_aux(Letras, [EspacosI|EspacosF], Espacos_Copia, Acc, Pals_Possiveis_Grelha) :-
    [EspacosI|EspacosF] \== [],
    palavras_possiveis_esp(Letras, Espacos_Copia, EspacosI, Pals_Possiveis_Espaco),
    append([EspacosI], [Pals_Possiveis_Espaco], Aux),
    append(Acc, [Aux], N_Acc),                                                                    %Enquanto a nossa lista dos espacos nao se encontra vazia, obter a lista de todas as palavras possiveis.
    palavras_possiveis_aux(Letras, EspacosF, Espacos_Copia, N_Acc, Pals_Possiveis_Grelha).  

%----------------------------------------------

%letras_comuns:
%INPUT: Lst_Pals (lista de listas de letras),
%OUTPUT: Letras_comuns (lista de pares (pos, letra) que contem letra na posicao pos).

letras_comuns([L_PalsI|L_PalsF], Letras_Comuns) :-
    length(L_PalsI, N),
    letras_comuns_aux([L_PalsI|L_PalsF], 1, N, [], Letras_Comuns),                                %Vamos percorrendo as letras da lista, desde o indice 1 ate ao comprimento da mesma,
    !.
    
letras_comuns_aux([L_PalsI|L_PalsF], L_Ind, L_Fim, Letras_Comuns, Res) :-
    L_Ind =< L_Fim,
    nth1(L_Ind, L_PalsI, El),
    eh_comum([L_PalsI|L_PalsF], L_Ind, El),
    append(Letras_Comuns, [(L_Ind, El)], Letras_Comuns_N),                                        %Se for comum, juntamos o tuplo ao acumulador e aumentamos o indice das letras da lista.
    L_Ind_Mais_1 is L_Ind + 1,
    letras_comuns_aux([L_PalsI|L_PalsF], L_Ind_Mais_1, L_Fim, Letras_Comuns_N, Res).
   
letras_comuns_aux([L_PalsI|L_PalsF], L_Ind, L_Fim, Letras_Comuns, Res) :-
    L_Ind =< L_Fim,
    L_Ind_Mais_1 is L_Ind + 1,
    letras_comuns_aux([L_PalsI|L_PalsF], L_Ind_Mais_1, L_Fim, Letras_Comuns, Res).

letras_comuns_aux(_, L_Ind, L_Fim, Res, Res) :-                                                 
    L_Ind >= L_Fim.                                                                               %Quando chegamos ao final da lista, unificamos o acumulador com o resultado.
 
eh_comum([], _, _).
eh_comum([L_PalsI|L_PalsF], L_Ind, El) :-                                                         %Eh_comum: Se o elemento no indice L_Ind da lista corresponde a El, tem sucesso. Caso contrario falha.
    nth1(L_Ind, L_PalsI, El),
    eh_comum(L_PalsF, L_Ind, El).


%----------------------------------------------
%PREDICADOS AUXILIARES: Acedem a elementos do tuplo.

acede_ind( (Ind,_), Ind).
acede_el( (_,El), El).

%----------------------------------------------

%atribui_comuns:
%Pals_Possiveis (lista de palavras possiveis), que e atualizada atribuindo a cada espaco as letras comuns.

atribui_comuns([]).
atribui_comuns([Pals_PossiveisI|Pals_PossiveisF]) :-
    atribui_aux(Pals_PossiveisI),
    atribui_comuns(Pals_PossiveisF).

atribui_aux(Pals_PossiveisI) :-
    acede_espaco(Pals_PossiveisI, Esp),
    acede_pals(Pals_PossiveisI, Pal),
    letras_comuns(Pal, Letras_Comuns),
    faz_atribuicoes(Esp, Letras_Comuns).


faz_atribuicoes(_, []).
faz_atribuicoes(PossiveisI, [Letras_ComunsI|Letras_ComunsF]) :-
    acede_ind(Letras_ComunsI, Ind),
    acede_el(Letras_ComunsI, El),
    nth1(Ind, PossiveisI, El, _),                                                                 %Utiliza se o predicado nth1 para relizar as atribuicoes
    faz_atribuicoes(PossiveisI, Letras_ComunsF).

%----------------------------------------------
%PREDICADOS AUXILIARES: Acede aos elementos de uma fila do puzzle: O seu espaco e a lista de palavras possiveis

acede_espaco([Esp,_ | _], Esp).
acede_pals([_,Pals | _], Pals).

%----------------------------------------------

%retira_impossiveis:
%Pals_Possiveis (lista de palavras possiveis),
%Novas_Pals_Possiveis (lista sem palavras impossiveis de Pals_Possiveis).

retira_impossiveis(Pals_Possiveis, Novas_Pals_Possiveis) :-
    retira_impossiveis_aux(Pals_Possiveis, [], Novas_Pals_Possiveis).               

retira_impossiveis_aux([], Acc, Acc).                                                           %Caso terminal 
retira_impossiveis_aux([Pals_PossiveisI | Pals_PossiveisF], Acc,Novas_Pals_Possiveis) :-
    filtra_fila(Pals_PossiveisI, Fila_Final),
    append(Acc, [Fila_Final], Acc_N),                                                           %Junta se a fila filtrada ao acumulador, que unifica com os resultados
    retira_impossiveis_aux(Pals_PossiveisF, Acc_N, Novas_Pals_Possiveis).

filtra_fila(Fila, Fila_Final) :-    %Recebe uma fila e filtra-a, retirando os impossiveis
    acede_espaco(Fila, Esp),
    acede_pals(Fila, Pals),
    unificam(Esp, Pals, [], Res),
    Fila_Final = [Esp | [Res]].

unificam(_, [],Res, Res).
unificam(Espaco, [PalavrasI | PalavrasF],Acc, Res) :-
    unifiable(Espaco, PalavrasI,_),
    append(Acc, [PalavrasI], Acc_N),
    unificam(Espaco, PalavrasF  , Acc_N, Res).

unificam(Espaco, [_ | PalavrasF], Acc, Res) :-
    unificam(Espaco, PalavrasF, Acc, Res).

%----------------------------------------------

%obtem_unicas:
%Pals_Possiveis (lista de palavras possiveis),
%Unicas (lista de palavras unicas de Pals_Possiveis).

obtem_unicas(Pals_Possiveis, Unicas) :-
    constroi_lista_unicas(Pals_Possiveis, [], Unicas).
    
constroi_lista_unicas([], Acc, Acc).
constroi_lista_unicas([Pals_PossiveisI|Pals_PossiveisF], Acc, Res) :-
    acede_pals(Pals_PossiveisI, Pals),
    length(Pals, 1),                                    %Se o tamanho da lista das palavras for 1, adicionar ao acumulador que unifica com o resultado
    append(Acc, Pals, Novo_Acc),
    constroi_lista_unicas(Pals_PossiveisF, Novo_Acc, Res).

constroi_lista_unicas([_|Pals_PossiveisF], Acc, Res) :- %Caso contrario, continuar com a recursao
    constroi_lista_unicas(Pals_PossiveisF, Acc, Res).

%----------------------------------------------

%retira_unicas:
%INPUT: Pals_Possiveis (lista de palavras possiveis),
%OUTPUT: Novas_Pals_Possiveis (Pals_Possiveis sem as palavras unicas).

retira_unicas(Pals_Possiveis, Novas_Pals_Possiveis) :-
    obtem_unicas(Pals_Possiveis, Unicas),               %Obtem a lista com as palavras unicas
    retira_unicas_aux(Pals_Possiveis, Unicas, [], Novas_Pals_Possiveis),
    !.

retira_unicas_aux([], _, Res, Res).
retira_unicas_aux([FilaI|FilaF], Unicas, Acc, Res) :-   %FilaI e F sao membos de pals possiveis, ex. [[s, P16, P17, P18], [[s, e, d, e], [s, o, a, r]]],
    acede_pals(FilaI, Pals),
    acede_espaco(FilaI, Esp),
    \+ length(Pals, 1),                                 %Se o tamanho nao for 1, retirar a intersecao das palalvras da fila com a lista das palavras unicas, sendo essas as palavras a remover,
    intersection(Pals, Unicas, Pals_Remover),
    subtract(Pals, Pals_Remover, Novas_Pals),           %Remover as palavras da repsetiva lista,
    append(Acc, [[Esp | [Novas_Pals]]], N_Acc),         
    retira_unicas_aux(FilaF, Unicas, N_Acc, Res).

retira_unicas_aux([FilaI|FilaF], Unicas, Acc, Res) :-
    append(Acc, [FilaI], N_Acc),                        %Caso contrario, manter a fila igual e adiciona-la ao acumulador.
    retira_unicas_aux(FilaF, Unicas, N_Acc, Res).

%----------------------------------------------

%simplifica:
%INPUT: Pals_Possiveis (lista de palavras possiveis),
%OUTPUT: Novas_Pals_Possiveis (Pals_Possiveis apos aplicar os predicados atribui_comuns, retira_impossiveis e retira_unicas, ate nao haver mais alteracoes).

simplifica(Pals_Correntes, Res) :-
    simplifica_aux(_, Pals_Correntes, Res).

simplifica_aux(Pals_Anteriores, Pals_Correntes, Res) :-         %Enquanto as listas nao sao iguais, modificar as palavras atuais e repetir o ciclo.
    Pals_Anteriores \== Pals_Correntes,
    modifica(Pals_Correntes, Novas_Pals_Correntes),
    simplifica_aux(Pals_Correntes, Novas_Pals_Correntes, Res).

simplifica_aux(Pals_Correntes, Pals_Correntes, Pals_Correntes).

modifica(Pals_Possiveis, Novas_Pals_Possiveis) :-
    atribui_comuns(Pals_Possiveis),
    retira_impossiveis(Pals_Possiveis, Pals_Possiveis_N),
    retira_unicas(Pals_Possiveis_N, Novas_Pals_Possiveis).

%----------------------------------------------

%inicializa(Puz, Pals_Possiveis):
%INPUT: Puz (puzzle),
%OUTPUT: Pals_Possiveis (lista de palavras possiveis de Puz simplificadas).

inicializa([Puz_Palavras, Puz_Grelha | _], Pals_Possiveis) :-   %Inicializa a lista de palavras simplificada para a resolucao do puzzle.
    obtem_letras_palavras(Puz_Palavras, Puz_Letras),
    espacos_puzzle(Puz_Grelha, Puz_Espacos),
    palavras_possiveis(Puz_Letras, Puz_Espacos, Puz_Pals_Possiveis),
    simplifica(Puz_Pals_Possiveis, Pals_Possiveis).

%----------------------------------------------

%escolhe_menos_alternativas:
%INPUT: Pals_Possiveis (lista de palavras possiveis), 
%OUTPUT: Escolha (elemento de Pals_Possiveis escolhido segundo o criterio indicado),
%Se todos os espacos em Pals_Possiveis tiverem associadas listas de palavras unitarias, o predicado devolve false.

escolhe_menos_alternativas(Pals_Possiveis, Escolha) :-
    filtra_unicas(Pals_Possiveis, [], Res),                     %Retira-se as palavras unicas da lista,
    nth1(1, Res, Res_Primeiro),                                 %Escolhe o primeiro elemento,              
    acede_pals(Res_Primeiro, Primeira_Pal),
    length(Primeira_Pal, Tam_Inicial),
    determina_menor_tamanho(Res, Tam_Inicial, Tam_Menor),       %Determina o menor tamanho,
    obtem_menor_tamanho(Res, Tam_Menor, Escolha).               %Retira o elemento com o menor tamanho,
    
filtra_unicas([], Res, Res).
filtra_unicas([Pals_PossiveisI|Pals_PossiveisF], Acc, Res) :-
    acede_pals(Pals_PossiveisI, Pals),
    \+ length(Pals, 1),
    append(Acc, [Pals_PossiveisI], Novo_Acc),
    filtra_unicas(Pals_PossiveisF, Novo_Acc, Res).

filtra_unicas([_|Pals_PossiveisF], Acc, Res) :-                 %Se e unica, ignorar e continuar a recursao
    filtra_unicas(Pals_PossiveisF, Acc, Res).

determina_menor_tamanho([], Tam_Menor, Tam_Menor).
determina_menor_tamanho([Possiveis_FiltradaI|Possiveis_FiltradaF], Tamanho_Menor, Res) :-
    acede_pals(Possiveis_FiltradaI, Pals),
    length(Pals, Tam_Pal),
    Tam_Pal < Tamanho_Menor,                                    %Se o tamanho obtido for menor que o anterior, continuar a recursao com o novo valor
    determina_menor_tamanho(Possiveis_FiltradaF, Tam_Pal, Res).

determina_menor_tamanho([_|Possiveis_FiltradaF], Tamanho_Menor, Res) :- %Caso contrario, continuar
    determina_menor_tamanho(Possiveis_FiltradaF, Tamanho_Menor, Res).

obtem_menor_tamanho([FiltradaI|_], Tam ,Escolha) :-
    acede_pals(FiltradaI, Pals),
    length(Pals, Tam),
    Escolha = FiltradaI.

obtem_menor_tamanho([FiltradaI|FiltradaF], Tam, Escolha) :-
    acede_pals(FiltradaI, Pals),
    \+ length(Pals, Tam),
    obtem_menor_tamanho(FiltradaF, Tam, Escolha).

%----------------------------------------------

%experimenta_pal:
%Pals_Possiveis (lista de palavras possiveis), 
%Escolha (elemento de Pals_Possiveis),
%Segue os seguintes passos:
    %1. Sendo Esp e Lst_Pals o espaco e a lista de palavras de Escolha, respectivamente, escolhe uma palavra de Lst_Pals, Pal. Utilize o predicado member para escolher esta palavra.
    %2. Unifica Esp com Pal.
    %3. Novas_Pals_Possiveis e o resultado de substituir, em Pals_Possiveis, o elemento Escolha pelo elemento [Esp, [Pal]].

experimenta_pal(Escolha, Pals_Possiveis, Novas_Pals_Possiveis) :-
    acede_pals(Escolha, Lst_Pals),
    acede_espaco(Escolha, Esp),
    member(Pal, Lst_Pals),
    substitui(Esp, Pal, Pals_Possiveis, [], Novas_Pals_Possiveis).

substitui(_, _, [], Res, Res).                  %Unificar o acumulador com o resultado no caso base
substitui(Espaco, Pal, [Pals_PossiveisI|Pals_PossiveisF], Acc, Novas_Pals_Possiveis) :-
    acede_espaco(Pals_PossiveisI, EspPals),
    Espaco \==  EspPals,                        %Se for diferente, juntar a fila original ao acumulador
    append(Acc, [Pals_PossiveisI], Acc_N),
    substitui(Espaco, Pal, Pals_PossiveisF, Acc_N, Novas_Pals_Possiveis).

substitui(Espaco, Pal, [Pals_PossiveisI|Pals_PossiveisF], Acc, Novas_Pals_Possiveis) :-
    acede_espaco(Pals_PossiveisI, EspPals),
    Espaco ==  EspPals,                         %Se o espaco e o pretendido, unificar o espaco com as palavras e juntar o resultado ao acumulador
    Espaco = Pal,
    append(Acc, [[Espaco, [Espaco]]], Acc_N),
    substitui(Espaco, Pal, Pals_PossiveisF, Acc_N, Novas_Pals_Possiveis).

%----------------------------------------------

%resolve_aux:
%INPUT: Pals_Possiveis (lista de palavras possiveis),
%OUTPUT: Novas_Pals_Possiveis (resultado de aplicar em Pals_Possiveis o algoritmo de resolucao indicado).

resolve_aux(Pals_Correntes, Res) :- 
    \+ nao_ha_possibilidades(Pals_Correntes),           %Enquanto existem possibilidades (existem listas que nao tem length 1), modificar as palavras possiveis
    modifica_pals(Pals_Correntes, Novas_Pals_Correntes),
    resolve_aux(Novas_Pals_Correntes, Res).
    
resolve_aux(Pals_Correntes, Pals_Correntes).

modifica_pals(Pals_Possiveis, Novas_Pals_Possiveis) :-
    escolhe_menos_alternativas(Pals_Possiveis, Escolha),
    experimenta_pal(Escolha, Pals_Possiveis, Pals_Possiveis_N),
    simplifica(Pals_Possiveis_N, Novas_Pals_Possiveis).

nao_ha_possibilidades([]).
nao_ha_possibilidades([Pals_PossiveisI|Pals_PossiveisF]) :-
    acede_pals(Pals_PossiveisI, Pals),
    length(Pals, 1),
    nao_ha_possibilidades(Pals_PossiveisF).

%----------------------------------------------

%resolve:
%INPUT/OUTPUT: Puz (puzzle),
%Resolve o puzzle.

resolve(Puz) :-
    inicializa(Puz, Pals_Possiveis),
    resolve_aux(Pals_Possiveis, _).