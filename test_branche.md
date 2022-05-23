# Projet-R : Modele ThreeMe

## Utilisation du Github


Github pour avoir un fichier R Markdown collaboratif

La connection entre Github et RMarkdwon peut être assez intéressante pour créer un fichier PDF final comprenant une partie texte et une partie graphique (potentiellement certains lignes de code) de manière harmonieuse. Si la question se pose un moment de reformater le document final sous Latex, RMarkdwon peut créer un fichier .tex, comprenant les graphiques et du texte. 

Voici la documentation que j'ai utilisée pour connecter RMarkdwon et ce requisitory Github (pour une utilisation ultérieure) : 
https://andrewmaclachlan.github.io/CASA0005repo/git-github-and-rmarkdown.html

Procédure actuelle à suivre : Pour utiliser R Studio et Github et modifier le requisitory présent, vous devez :
 - envoyer à Bijan votre mail git pour être ajouté en tant que collaborateur
 -  Installer Github Desktop
 -  Sur le requisitory présent (site web), cliquez sur le bouton Code > Open with Github Desktop
 -  Une fois ouvert dans Github Desktop, vous cliquez sur "Clone". Vous avez une copie locale du requisitory sur votre ordinateur. 
 -  Une fois la copie locale effectuée, à partir de github desktop, vous pouvez ouvrir le projet R grâce à "Open in R Studio". 
     -  Vous pouvez, par exemple, ouvrir le test_file, puis écrire une nouvelle ligne "test_yourName" 
     -  Enregistrer le changement sur Rstudio (fichier+projet) et fermer Rstudio 
     -  Cliquer, dans la copie locale de Github Desktop, sur "Commit to Master" en donnant un nom au commit ('update' par exemple)
     -  Cliquer, dans la copie locale de Github Desktop, sur "Push Origin" pour envoyer le changement sur le requisitory commun GitHub 
 -  Vous pouvez Pull sur Github Desktop les changements faits par d'autres utilisateurs. Si Github Desktop ne vous le propose pas automatiquement, vous pouvez cliquer sur le bouton refresh "Fetch Origin", puis "Pull origin"

## RMarkdown

Syntaxe Markdown : 
https://bookdown.org/yihui/rmarkdown/markdown-syntax.html

Les blocs de code peuvent être intégrées grâce au bouton +C. Ces blocs de code peuvent être exécutées par R, sans être inclus dans le PDF final. C'est intéressant pour afficher des graphiques de manière simple, en profitant de l'environnement commun créé par le fichier .proj 

Certaines commandes Latex peuvent être intégrées grâce à l'usage du compilateur Lualuatex. 
Pour knit un fichier .rmd en pdf, il faut avoir une distribution de Latex sur son ordinateur (Miktex, Tex Live...) : j'utilise Miktex (je dois désactiver mon antivirus pour knit en pdf). 


## Warning

Github permet de créer des fichiers .rmd collaboratifs, néanmoins ce n'est pas un googledoc. Les fichiers ne s'actualisent pas en temps continu. IL peut y avoir des conflits entre "Push". Si on travaille en même temps sur un même fichier, on peut push certaines parties du code antérieures, alors qu'elles viennent d'être modifiées par quelqu'un d'autre. Les conflits se résolvent, mais ça peut être désagréable. Recommandation : 

- sauvegarder son travail sur une copie locale en dehors de Github

- prévenir les autres lorsqu'ils travaillent ou push une nouvelle version. 

- pull et push régulièrement afin d'être assurée de travailler sur des versions actualisées du code. 

L'utilisation reste quand même meilleur qu'un drive. 

## Presentation du projet

Proposition de sujet de projet pour le parcours modélisation, master EEET 2021-2022 Encadrement : Meriem Hamdi-Cherif (OFCE SciencesPo)

Au-delà de la question de l’adaptation qui représente un véritable enjeu socio-économique pour les pays en développement, les questions relatives aux politiques de réduction des émissions de gaz à effet de serre (GES) et leurs enjeux socio-économiques sont tout aussi centrales, en particulier dans les arènes des négociations de la COP26 qui se déroule actuellement à Glasgow. Face à l’urgence de l’action climatique et aux nouvelles exigences relatives à la nécessité d’atteindre un équilibre entre les émissions et les absorptions en 2050 (accord de Paris), les gouvernements des différents pays doivent entreprendre des réformes structurelles et mettre en place les conditions propices pour favoriser un développement socioéconomique faible en émissions de gaz à effet de serre (GES) et résilient au changement climatique. L’élaboration de la Stratégie Nationale Bas Carbone (SNBC) peut représenter pour les pays en développement, et la Tunisie en particulier une opportunité pour redynamiser la croissance économique tout en réduisant les émissions de GES. A l’OFCE (l’Observatoire Français des Conjonctures Economiques – Sciences Po), en collaboration avec des institutions Tunisiennes, des scénario « SNBC Tunisie » ont été produits avec le modèle d’équilibre général calculable ThreeME-Tunisie afin de supporter la mise en place de politiques climatiques et énergétiques dans le cadre de la SNBC Tunisienne.

ThreeME est un modèle hybride en ce sens qu’il permet de combiner la modélisation macroéconomique (dite top-down) et la modélisation technique des consommations énergétiques (dite bottom-up). C’est un modèle macroéconomique multisectoriel conçu pour représenter l’économie d’un pays ou d’une région et évaluer les impacts économiques de politiques environnementales et énergétiques à moyen et long terme au niveau macroéconomique global mais aussi sectoriel. Enfin ThreeME est un modèle d’équilibre général néokeynésien qui autorise des déséquilibres de court terme avec la possibilité d’une utilisation sous-optimale des facteurs de production (e.g. possibilité de chômage involontaire), ce qui le rend particulièrement pertinent pour étudier les problématiques liées à la transition énergétique. L’objectif de ce projet est de produire une analyse macroéconomique d’un certain nombre de scénarios qui s’inscrivent dans le cadre de la SNBC tunisienne. En particulier, il sera question d’analyser 

- un scénario qui étudie la levée des subventions aux énergies fossiles, 

- un scénario qui se concentre sur l’analyse de l’introduction d’une taxe carbone dans l’économie tunisienne, 

-  un scénario où l’on impose la pénétration d’énergies renouvelables de façon significative dans le mix électrique 

-   et enfin, un dernier scénario où l’on considère un package de politiques qui intègre les politiques mises en œuvre dans les trois derniers scénarios. 


Afin d’effectuer une évaluation macroéconomique de la mise en place de telles politiques, les étudiants devront d’une part réfléchir aux indicateurs pertinents à observer et à analyser, et d’autre part comprendre et exhiber certains mécanismes importants lors de l'application des politiques énergétiques et climatiques. En outre, ce projet permettra aussi aux étudiants de se confronter à la réalité de l’utilisation d’un modèle comme outils d’aide à la décision et support à la formulation de politiques publiques dans le cadre de la transition énergétique et bas carbone. Donc au-delà du pur aspect quantitatif et de l’analyse des sorties du modèle, un accent particulier sera mis aussi sur la façon de « raconter l’histoire » et les messages à formuler afin de proposer des supports de réflexion pour faciliter la mise en œuvre de la transition énergétique et bas carbone en Tunisie.
