#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/******************************************************************************/
/********************************* Code fourni ********************************/
/******************************************************************************/

typedef enum { B, H, D, G } direction;

typedef struct {
  int taille;
  int *origine_x;
  int *origine_y;
  int *largeur;
  int *parent;
  int *nb_enfants;
  int **enfants;
} hierarchie;

/*** Fonction d'affichage d'un tableau ***/
void afficher_tableau(int *tab, int taille) {
  if (taille == 0) {
    printf("[]\n");
  } else {
    printf("[%d", tab[0]);
    for (int i = 1; i < taille; i++) {
      printf(", %d", tab[i]);
    }
    printf("]\n");
  }
}

/*** Fonction d'affichage d'une hierarchie ***/
void afficher_hierarchie(hierarchie h) {

  printf("origine_x : ");
  afficher_tableau(h.origine_x, h.taille);

  printf("origine_y : ");
  afficher_tableau(h.origine_y, h.taille);

  printf("largeur : ");
  afficher_tableau(h.largeur, h.taille);

  printf("parent : ");
  afficher_tableau(h.parent, h.taille);

  printf("enfants : \n");
  for (int i = 0; i < h.taille; i++) {
    afficher_tableau(h.enfants[i], h.nb_enfants[i]);
  }
  printf("\n");
}

/*** Fonction d'affichage graphique d'une grotte, avec les niveaux d'eau
     indiqués dans le tableau hauteurs (calculé en question 16) ***/
void dessiner_eau(hierarchie h, float *hauteurs) {
  int largeur = h.largeur[0];
  float *ligne = malloc(sizeof(float) * largeur);

  bool continuer = true;
  int y = 1;

  while (continuer) {
    continuer = false;
    for (int x = 0; x < largeur; x++) {
      ligne[x] = -1.0;
    }
    for (int i = 0; i < h.taille; i++) {
      if (h.origine_y[i] == y) {
        continuer = true;
        for (int x = 0; x < h.largeur[i]; x++) {
          ligne[h.origine_x[i] + x] = hauteurs[i];
        }
      }
    }
    printf("▓▓");
    for (int x = 0; x < largeur; x++) {
      if (ligne[x] < 0.0)
        printf("▓▓");
      else if (ligne[x] >= 1.0)
        printf("██");
      else if (ligne[x] >= 0.875)
        printf("▇▇");
      else if (ligne[x] >= 0.750)
        printf("▆▆");
      else if (ligne[x] >= 0.625)
        printf("▅▅");
      else if (ligne[x] >= 0.500)
        printf("▄▄");
      else if (ligne[x] >= 0.375)
        printf("▃▃");
      else if (ligne[x] >= 0.250)
        printf("▂▂");
      else if (ligne[x] >= 0.125)
        printf("▁▁");
      else
        printf("  ");
    }
    printf("▓▓\n");

    y++;
  }
  free(ligne);
}

/******************************************************************************/
/**************************** Grottes à ciel ouvert ***************************/
/******************************************************************************/

/*** Question 10 ***/
int nombre_B(direction profil[], int taille_profil) {
  int count = 0;
  for (int i = 0; i < taille_profil; i++) {
    if (profil[i] == B)
      count++;
  }
  return count;
}

/*** Question 11 ***/
hierarchie init_hierarchie(int taille) {
  hierarchie h;
  h.taille = taille;
  h.largeur = malloc(sizeof(int) * taille);
  h.nb_enfants = malloc(sizeof(int) * taille);
  h.origine_x = malloc(sizeof(int) * taille);
  h.origine_y = malloc(sizeof(int) * taille);
  h.parent = malloc(sizeof(int) * taille);
  h.enfants = malloc(sizeof(int *) * taille);
  for (int i = 0; i < taille; i++) {
    h.enfants[i] = malloc(sizeof(int) * taille);
  }

  return h;
}

/*** Question 12 ***/
void liberer_hierarchie(hierarchie h) {
  free(h.largeur);
  free(h.nb_enfants);
  free(h.origine_x);
  free(h.origine_y);
  free(h.parent);
  for (int i = 0; i < h.taille; i++) {
    free(h.enfants[i]);
  }
  free(h.enfants);
}

/******************************************************************************/
/****************************** Structure de pile *****************************/
/******************************************************************************/

typedef struct {
  int *contenu; /* tableau contenant les éléments */
  int capacite; /* le nombre d'éléments maximal que la pile peut stocker */
  int taille;   /* le nombre d'éléments dans la pile : initialement 0 */
} pile;

pile *creer_pile(int capacite) {
  pile *p = malloc(sizeof(pile));
  p->contenu = calloc(capacite, sizeof(int));
  assert(p && p->contenu);
  p->capacite = capacite;
  p->taille = 0;
  return p;
}

void liberer_pile(pile *p) {
  free(p->contenu);
  free(p);
}

bool est_vide(pile *p) { return p->taille == 0; }

void empiler(pile *p, int v) {
  assert(p->taille < p->capacite);
  p->contenu[p->taille++] = v;
}

int depiler(pile *p) {
  assert(p->taille > 0);
  p->taille--;
  return p->contenu[p->taille];
}

int sommet(pile *p) {
  assert(p->taille > 0);
  return p->contenu[p->taille - 1];
}

void print_stack(pile *p) {
  for (int i = 0; i < p->taille; i++) {
    printf("%d, ", p->contenu[i]);
  }
  printf("\n");
}

/*** Question 13 : exécuter cette fonction ne doit pas provoquer d'erreur ***/
void test_pile() {
  pile *p = creer_pile(10);
  empiler(p, 12);
  assert(!est_vide(p));
  empiler(p, 7);
  empiler(p, 15);
  empiler(p, 10);
  assert(depiler(p) == 10);
  assert(depiler(p) == 15);
  empiler(p, 42);
  empiler(p, 43);
  assert(depiler(p) == 43);
  assert(depiler(p) == 42);
  assert(!est_vide(p));
  assert(depiler(p) == 7);
  assert(depiler(p) == 12);
  assert(est_vide(p));
  liberer_pile(p);

  printf("Bravo, tous les tests ont été passés !\n");
}

/*** Question 14 ***/
hierarchie construire_hierarchie(direction profil[], int taille_profil) {
  int rect_idx = 0;

  // (0, 0)
  int x = 0;
  int y = 0;

  pile *stack = creer_pile(taille_profil);
  int taille_hierarchie = nombre_B(profil, taille_profil);
  hierarchie h = init_hierarchie(taille_hierarchie);

  for (int i = 0; i < taille_profil; i++) {

    if (profil[i] == B) {
      y++;
      h.origine_x[rect_idx] = x;
      h.origine_y[rect_idx] = y;
      h.largeur[rect_idx] = -1;
      h.nb_enfants[rect_idx] = 0;
      int parent_idx = est_vide(stack) ? -1 : sommet(stack);
      h.parent[rect_idx] = parent_idx;
      if (parent_idx >= 0) {
        h.enfants[parent_idx][h.nb_enfants[parent_idx]] = rect_idx;
        h.nb_enfants[parent_idx]++;
      }
      empiler(stack, rect_idx);
      rect_idx++;
    } else if (profil[i] == H) {
      y--;
      assert(!est_vide(stack));
      int popped = depiler(stack);
      h.largeur[popped] = x - h.origine_x[popped];

    } else if (profil[i] == D)
      x++;
  }

  return h;
}

bool are_equal(int *arr1, int *arr2, int size) {
  for (int i = 0; i < size; i++) {
    if (arr1[i] != arr2[i])
      return false;
  }
  return true;
}

void print(int *arr, int size) {
  for (int i = 0; i < size; i++) {
    printf("%d, ", arr[i]);
  }
}

void test_hierarchie() {
  int taille_profil_1 = 35;
  direction profil_1[35] = {B, B, B, D, H, D, B, B, D, H, H, H,
                            D, B, B, D, H, D, B, B, D, H, D, B,
                            D, H, H, D, B, B, D, H, H, H, H};
  //(0, 0)
  //|         __                     |
  //|   __   |  |   __          __   |
  //|__|  |  |  |__|  |   __   |  |  |
  //      |__|        |__|  |__|  |__|

  hierarchie h = construire_hierarchie(profil_1, taille_profil_1);
  assert(h.taille == 12);

  // Tests largeur
  assert(h.largeur[0] == 11);
  assert(h.largeur[1] == 3);
  assert(h.largeur[2] == 1);
  assert(h.largeur[3] == 1);
  assert(h.largeur[4] == 1);
  assert(h.largeur[5] == 7);
  assert(h.largeur[6] == 1);
  assert(h.largeur[7] == 3);
  assert(h.largeur[8] == 1);
  assert(h.largeur[9] == 1);
  assert(h.largeur[10] == 1);
  assert(h.largeur[11] == 1);

  // Tests enfants
  assert(are_equal(h.enfants[0], (int[]){1, 5}, 2));
  assert(are_equal(h.enfants[1], (int[]){2, 3}, 2));
  assert(are_equal(h.enfants[2], (int[]){}, 0));
  assert(are_equal(h.enfants[3], (int[]){4}, 1));
  assert(are_equal(h.enfants[4], (int[]){}, 0));
  assert(are_equal(h.enfants[5], (int[]){6, 7, 10}, 3));
  assert(are_equal(h.enfants[6], (int[]){}, 0));
  assert(are_equal(h.enfants[7], (int[]){8, 9}, 2));
  assert(are_equal(h.enfants[8], (int[]){}, 0));
  assert(are_equal(h.enfants[9], (int[]){}, 0));
  assert(are_equal(h.enfants[10], (int[]){11}, 1));
  assert(are_equal(h.enfants[11], (int[]){}, 0));

  // Autres tests
}

/******************************************************************************/
/*************************** Remplissage de la grotte *************************/
/******************************************************************************/

/*** Question 15 ***/

pile *spill_stack(pile *p) {
  pile *other = creer_pile(p->capacite);
  assert(p);
  while (!est_vide(p)) {
    empiler(other, depiler(p));
  }
  return other;
}

// Inner
void _dfs(hierarchie h, pile *p, int rect) {
  if (h.nb_enfants[rect] == 0) {
    empiler(p, rect);
    return;
  } else {
    for (int i = 0; i < h.nb_enfants[rect]; i++) {
      _dfs(h, p, h.enfants[rect][i]);
    }
    empiler(p, rect);
    return;
  }
}

pile *dfs(hierarchie h) {
  pile *p = creer_pile(h.taille);
  assert(h.parent[0] == -1);
  _dfs(h, p, 0);
  return p;
}

pile *ordre_remplissage_depuis_origine(hierarchie h) {
  pile *ordre = dfs(h); // ici : remplacer NULL par une allocation dynamique
  return ordre;
}

void test_dfs() {
  int taille_profil_1 = 35;
  direction profil_1[35] = {B, B, B, D, H, D, B, B, D, H, H, H,
                            D, B, B, D, H, D, B, B, D, H, D, B,
                            D, H, H, D, B, B, D, H, H, H, H};
  //(0, 0)
  //|         __                     |
  //|   __   |  |   __          __   |
  //|__|  |  |  |__|  |   __   |  |  |
  //      |__|        |__|  |__|  |__|

  hierarchie h = construire_hierarchie(profil_1, taille_profil_1);
  int *ordre = ordre_remplissage_depuis_origine(h)->contenu;
  assert(are_equal(ordre, (int[]){2, 4, 3, 1, 6, 8, 9, 7, 11, 10, 5, 0},
                   h.taille));
}

/*** Question 16 ***/

float clamp_to_one_exceed(float a) { return (a >= 1.0) ? 1.0 : a; }
float clamp_to_zero_exceed(float a) { return (a <= 0.0) ? 0.0 : a; }
float *hauteurs_eau_depuis_origine(hierarchie h, float t) {
  float *hauteur = malloc(sizeof(float) * h.taille);
  int *ordre = ordre_remplissage_depuis_origine(h)->contenu;
  assert(hauteur);
  float remaining_time = t;
  for (int i = 0; i < h.taille; i++) {
    int idx = ordre[i];
    hauteur[idx] = clamp_to_one_exceed(remaining_time / (float)h.largeur[idx]);
    remaining_time =
        clamp_to_zero_exceed(remaining_time - (float)h.largeur[idx]);
  }
  return hauteur;
}

/*** Question 17 ***/
/* Votre réponse ici :
Il se présente deux cas qu'il faut étudier séparément et précisément:
    -cas vertical: il est trivial, au vu des règles d' "écoulement" de l'eau, que deux rectangles à des altitudes y1 et y2 différentes
    ne se rempliront pas simltanément
    -cas horizontal: si les deux rectangles se situent à la même altitude y, alors puisque ce sont deux rectangles distincts,
    ils sont nécessairement séparés par une section de mur. Or, si la 




*/

/*** Question 18 ***/
int *volumes_totaux(hierarchie h) {
  int *volume = NULL; // ici : remplacer NULL par une allocation dynamique

  /*** A compléter ***/

  return volume;
}

void print_arr(float *arr, size_t size) {
  for (int i = 0; i < size; i++) {
    printf("%f, ", arr[i]);
  }
  printf("\n");
}

int main() {

  int taille_profil_1 = 35;
  direction profil_1[35] = {B, B, B, D, H, D, B, B, D, H, H, H,
                            D, B, B, D, H, D, B, B, D, H, D, B,
                            D, H, H, D, B, B, D, H, H, H, H};

  assert(nombre_B(profil_1, taille_profil_1) == 12);

  test_pile();

  hierarchie h = construire_hierarchie(profil_1, taille_profil_1);
  afficher_hierarchie(h);

  print_stack(ordre_remplissage_depuis_origine(h));
  test_dfs();
  test_hierarchie();
  print_arr(hauteurs_eau_depuis_origine(h, 15.0), h.taille);
  

  dessiner_eau(h, hauteurs_eau_depuis_origine(h, 13.0));
  liberer_hierarchie(h);

    return 0;
}