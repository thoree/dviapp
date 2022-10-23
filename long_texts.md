# Introduction_long_1

This app deals with Disaster Victim Identification (DVI) problems and
power calculation for kinship problems. Our goal has been to make
available functionality in the `pedsuite` of R libraries and also the
`dvir` library. We also expand on functionality in the [Familias
software.](https://www.familias.no/)

There are three modules, all based on built in cases or user data
(Familias or R files):

-   **Power:** Simulations can be done to determine if goals are likely
    to be achieved.
-   **Priority:** The aim is to find the optimal extra persons to
    genotype.
-   **DVI:** Methods to include or exclude missing persons are provided.

For more information, check the books: [Mass
identications,](https://www.elsevier.com/books/mass-identifications/kling/978-0-12-818423-3)
(Kling et al., 2021), [Pedigree Analysis in
R](https://www.elsevier.com/books/pedigree-analysis-in-r/vigeland/978-0-12-824430-2)
(Vigeland, 2021), and the [dvir
paper](https://www.nature.com/articles/s41598-021-93071-5) (Vigeland and
Egeland, 2021). For further documentation and bug reporting, please go
[here](https://github.com/thoree/dviapp).

---

Esta aplicación se ocupa de los problemas de identificación de víctimas
de desastres (DVI) y del cálculo del poder estadístico en problemas de
parentesco. Nuestro objetivo ha sido hacer que la funcionalidad de las
bibliotecas R `pedsuite` y `dvir` sea fácilmente accesible.

También ampliamos la funcionalidad del [*software
Familias*](https://www.familias.no/). La aplicación se compone de tres
módulos, todos ellos con casos pre-configurados o con la posibilidad de
que los usuarios analicen sus propios datos (archivos Familias o R):

-   **Poder:** permite realizar simulaciones para determinar si es
    probable que se alcancen los objetivos en un proyecto de
    identificación.
-   **Prioridad**: Permite determinar qué familiares de referencia extra
    son los más óptimos para genotipar en casos de pedigríes
    incompletos.
-   **DVI**: proporciona métodos para incluir o excluir personas
    desaparecidas.

Para más información, consulte los libros: [*Mass
identications*](https://www.elsevier.com/books/mass-identifications/kling/978-0-12-818423-3),
(Kling et al., 2021), [*Pedigree Analysis in
R*](https://www.elsevier.com/books/pedigree-analysis-in-r/vigeland/978-0-12-824430-2)
(Vigeland, 2021) y el artículo
"[*dvir*](https://www.nature.com/articles/s41598-021-93071-5)" (Vigeland
y Egeland, 2021). Para obtener más documentación y reportar errores, por
favor vaya [*aquí*](https://github.com/thoree/dviapp).

# Power_long_1

LR comparing H1: `MP and REF full brothers`, versus H2: `MP and REF`
unrelated, has been computed for 1000 simulations of MP and REF
conditioned on H1 below. The simulations use the 35 markers in the
database `NorwegianFrequencies` documented in the R library forrel. In
`Power > Analyses based on built in cases` some prepared cases can be
run and parameters like the number of markers, can be changed. In
`Power > Analyses based on user loaded data`, similar output is
obtained by loading a familias file prepared by the user. The
simulations will be conditioned on genotyped individuals, if any.

---

En la figura se pueden ver los valores del LR para las hipótesis H1:
'MP y REF son hermanos completos', versus H2: 'MP y REF' no están
relacionados. Para ello se han realizado 1000 simulaciones de perfiles
genéticos de MP y REF teniendo en cuenta que H1 es cierta. Se usaron los
35 marcadores de la base de datos `NorwegianFrequencies` documentada
en la biblioteca R forrel. En la pestaña `Potencia > Análisis basados
en casos pre-configurados` se pueden ejecutar algunos casos preparados
y se pueden cambiar parámetros como el número de marcadores. En la
pestaña `Potencia > Análisis basados en datos cargados por el
usuario`, se obtiene un resultado similar cargando un archivo familias
preparado por el usuario. Las simulaciones estarán condicionadas a
individuos genotipados, si los hubiere.

# Power_long_2

The missing person should be named `MP` and the reference `REF` in
the file. The
file [**BrotherPower.fam**](https://familias.name/dviapp/BrotherPower.fam) gives
output similar to that in `Power > Explanations` (but not identical,
even for the same seed, since the simulation implementation is not
identical). Genotyped individuals (if any) are hatched and first marker
displayed in the plot and these individuals will be conditioned on.
Here's an example
file [**BrotherPowerConditioned.fam**](https://familias.name/dviapp/BrotherPowerConditioned.fam)

---

En el archivo que desee subir, la persona desaparecida debe llevar el
nombre `MP` y la de referencia el nombre `REF`. El archivo
[*BrotherPower.fam*](https://familias.name/dviapp/BrotherPower.fam) da
un resultado similar al que se muestra en la ventana `Poder >
Explicaciones` (aunque no da un resultado idéntico, ya que aunque se
use la misma semilla, la simulación no es idéntica). Los individuos
genotipados (si los hay) aparecen sombreados y en la gráfica se muestra
el genotipo del primer marcador. Las simulaciones se condicionarán a
estos genotipos. Puede encontrar un archivo a modo de ejemplo en el link
[*BrotherPowerConditioned.fam*](https://familias.name/dviapp/BrotherPowerConditioned.fam)

# Prioritise_long_1

The below explanation applies to the example obtained if 'brother'
(default) is chosen in the pull down menu below. The LR comparing H1:
`MP and REF full brothers`, to H2: `MP and REF unrelated`, has been
computed for 100 unconditional simulations of MP and REF conditioned on
H1 below. This corresponds to the `REF` case in the panel to the
right. We see that we can expect no exclusions (in fact exclusions are
impossible with only two brothers) and log10(LR) slightly exceeding 10.
If one brother, `E1` is genotyped we can expect more than 10
exclusions and a log10(LR) slightly exceeding 20. Finally, if both
brothers `E1`and `E2` are genotyped, the expected number of
exclusions and LR increase further. 10 profiles are simulated for the
relatives ('REF', `E1` and `E2`), assuming H1. For each of these
10 profiles, corresponding to the smaller circles, 1000 simulations are
performed for `MP` under H1 and H2. In `Prioritise > Analyses based
on built in cases` simulations can be performed for various parameter
choices. In `Prioritise > Analyses based on user loaded data` similar
simulations can be performed from a fam file.

---

La figura muestra el resultado cuando se selecciona el ejemplo
'hermano' (predeterminado) en el menú desplegable de abajo. Para
calcular el LR teniendo en cuenta las hipótesis H1: 'MP y REF son
hermanos completos', con H2: 'MP y REF no están relacionados', se
realizaron 100 simulaciones incondicionales de perfiles genéticos de MP
y REF suponiendo que H1 es cierta. El resultado se muestra en el caso
`REF` del panel de la derecha. Vemos que no podemos esperar
exclusiones (de hecho, las exclusiones son imposibles con solo dos
hermanos) y el log10 (LR) es ligeramente superior a 10. Si se genotipa
un hermano más, 'E1', podemos esperar más de 10 exclusiones y un log10
(LR) ligeramente superior a 20. Finalmente, si se genotipan dos hermanos
más, `E1` y `E2`, el número esperado de exclusiones y el valor del
LR aumentan aún más. Se han simulado 10 perfiles para los familiares
('REF', `E1` y `E2`), asumiendo que H1 es cierta. Para cada uno de
estos 10 perfiles, correspondientes a los círculos más pequeños, se
realizan 1000 simulaciones de perfiles para `MP` bajo H1 y H2. En la
pestaña `Priorizar > Análisis basados ​​en casos integrados` se
pueden realizar simulaciones para varias opciones de parámetros. En la
pestaña `Priorizar > Análisis basados ​​en datos cargados por el usuario` se pueden realizar simulaciones similares desde un archivo
fam.

# Prioritise_long_2

REMOVE :Priority power is calculated by uploading a Familias file.
Here's an
example: [**BrotherPriority.fam**](https://familias.name/dviapp/BrotherPriority.fam).

The missing person should be named `MP`, the reference `REF`, and
the extra candidates for genotyping `E1`and `E2`. The mentioned file
gives output similar to that in `Priority > Explanations` (but not
identical, even for the same seed, since this is simulation).

---

Para usar esta herramienta es necesario cargar un archivo de Familias.
Puede encontrar un ejemplo en el link:
[*BrotherPriority.fam*](https://familias.name/dviapp/BrotherPriority.fam).

La persona desaparecida debe nombrarse `MP`, la de referencia `REF`
y los candidatos adicionales para genotipado `E1` y `E2`. El archivo
mencionado da un resultado similar al que aparece en la ventana
`Priorizar > Explicaciones` (aunque no idéntico, incluso para la
misma semilla, ya que se trata de una simulación).

# DVI_long_1

Analyses can be done in this module from built in cases, from Familias
(`fam`) files or from R data. The below figure shows the planecrash
data. When the data is loaded in `DVI > Analyses based on built in
cases`, the following summary is provided:, `DVI data. 8 victims: V1, ..., V8. 5 missing. 5 typed refs: R1, ..., R5 . 5 reference
families.` The data is also available as a fam file:
[planecrash.fam](https://familias.name/dviapp/planecrash.fam) , and can
also be downloaded as RData:
[planecrash.RData](https://familias.name/dviapp/planecrash.RData) . See
the documentation for the details on the five analyses implemented. Here
we only provide brief explanations:

-   **IBD estimates:** The pairwise relationship between all pairs of
    victims is estimated.
-   **Exclusion:** Each victim is tried as each missing person and the
    number of exclusions is given.
-   **Pairwise LR:** For each victim V and each missing person M, the LR
    comparing `V = M` to `V and M unrelated` is calculated.
-   **Joint:** All possible assignments of victims to missing persons
    are evaluated and solutions ranked according to the likelihood.
-   **Posterior:** Computes posterior pairing probabilities, i.e., the
    probability that a victim V is the missing person M.

----

En este módulo, los análisis se pueden hacer casos pre-configurados,
desde archivos de Familias (`fam`) o desde datos R. La figura
siguiente muestra los datos del caso "accidente aéreo''. Cuando los
datos se cargan en la pestaña `DVI > Análisis basados ​​en casos
integrados`, la aplicación proporciona el siguiente resumen: `Datos
DVI. 8 víctimas: V1, ..., V8 . 5 desaparecidos. 5 referencias escritas:
R1, ..., R5. 5 familias de referencia.` Los datos también están
disponibles en un archivo con formato fam:
[*planecrash.fam*](https://familias.name/dviapp/planecrash.fam), y
también se pueden descargar como RData:
[*planecrash.RData*](https://familias.name/dviapp/planecrash.RData).
Consulte la documentación para obtener detalles sobre los cinco análisis
implementados. Aquí sólo proporcionamos breves explicaciones:

-   **Estimaciones IBD**: se estima la relación entre todas las víctimas
    por pares.
-   **Exclusión**: Se coloca a cada víctima en la posición de cada
    desaparecido en cada pedigrí y se da el número de exclusiones.
-   **LR por pares**: para cada víctima V y cada persona desaparecida M,
    se calcula el LR teniendo en cuenta las hipótesis `V = M` y `V y
    M sin parentesco`.
-   **Conjunto**: se evalúan todas las asignaciones posibles de víctimas
    a personas desaparecidas y las soluciones se clasifican según su
    probabilidad.
-   **Posterior**: calcula las probabilidades *a posteriori* de
    emparejamiento, es decir, la probabilidad de que una víctima V sea
    la persona desaparecida M.
    
# DVI_long_2

If there are multiple missing persons in a family, like in the case
based on
the [**FamilyWith3Missing.fam**](https://familias.name/dviapp/FamilyWith3Missing.fam) ,
which is similar to a built-in-case, the number of missing must be
specified in `Settings`, (`No missing :3`), and the missing persons
must be named `M1`, `M2`, ... (this is the case in the linked
fam-file).

Si hay varias personas desaparecidas en una familia, como en el caso
basado en
[**FamilyWith3Missing.fam**](https://familias.name/dviapp/FamilyWith3Missing.fam),
que es similar a un caso pre-configurado, el número de personas
desaparecidas debe especificarse en `Configuración` (`Núm. de
desaparecidos: 3`), y las personas desaparecidas deben denominarse
`M1`, `M2`, ... (como en el archivo fam vinculado).
