n_for_param_tuples <- function() {
  # The reason we use a function (instead of hardcoding -1 in one spot) is
  # to make it clear inside this file why "n" is -1 sometimes.
  #
  # Keep as negative so can be sure it is not the same as an element of nvec.
  # A positive number could cause the seed to be the same as when run with
  # do_mc_run(). It likely wouldn't cause a problem in practice in most
  # situations, but we might as well set the seed for gen_param_tuples()
  # independent of do_mc_run().
  #
  # Note that in gen_param_tuples(), the phash does change depending on
  # the parameter. It is just the "n" that will always be -1.
  #
  # Could potentially use the position of the dgpp in the dgpp_vec, but
  # we often want the hash invariant to the position.
  return(-1)
}


seed_init_noop <- function(base_seed, n, phash) {
  # returning NULL means "don't set the seed" to the caller.
  return(NULL)
}


# this is a bad approach. I just keep it in case I want to
# test bad pnhashes to show why the "good" one is better.
#' @importFrom stats rnorm
pnhasher_naive_sum <- function(base_seed, n, phash) {
  entering_seed <- .Random.seed

  assign(".Random.seed", base_seed, .GlobalEnv)

  rnorm(n)

  # (https://stackoverflow.com/questions/59650584/r-convert-a-word-to-numbers)
  # (need to subtract 96 if we want the position in the alphabet).
  #  > base::utf8ToInt("hello")
  #  [1] 104 101 108 108 111
  phash_int <- sum(base::utf8ToInt(phash))
  # todo: with a long string, does this take a lot of RAM?
  rnorm(phash_int)

  #seed_ <- base_seed
  seed_ <- .Random.seed

  # restore seed
  assign(".Random.seed", entering_seed, .GlobalEnv)
  return(seed_)
}


# essentially what we use here is a hash that has a specific range (the range
# of arguments accepted by set.seed())
# other possible ways to hash:
# https://stackoverflow.com/questions/14365911/hashing-function-for-mapping-integers-to-a-given-range
# https://stackoverflow.com/questions/47542719/r-fast-hashing-of-strings-to-integer-modulo-n
# https://stackoverflow.com/questions/3470447/can-i-use-a-list-as-a-hash-in-r-if-so-why-is-it-so-slow
# https://github.com/eddelbuettel/digest/issues/82
#
# We specify in DESCRIPTION that we need at least version 0.6.19 since that interoduces digest2int.
#' @importFrom digest digest2int
#' @importFrom digest digest
pnhasher_digest2int <- function(n, phash) {
  # TODO: how long the integer can be depends on the RNG. Condition on the RNG?

  # todo: check .Machine$integer.max instead.
  #       Actually no, that's a bad idea. Then the hash would depend on .Machine$integer.max.
  #       that value can change from machine to machine (and can be manually changed as well).
  #       Maybe check .Machine$integer.max only to give an *error* if too low.
  # 2e9 is just less than the max that the 'seed' argument of digest2int() seems
  # to support. Could use the "else" method for both cases,
  # but this seems prefered.
  if (n <= 2e9) {
    # https://stackoverflow.com/questions/27442991/how-to-get-a-hash-code-as-integer-in-r
    #
    # Output can be negative, which is expected since set.seed() accepts a signed
    # integer.
    #
    # > digest::digest2int("blah", seed = 5)
    # [1] -638703268
    hash_ <- digest::digest2int(phash, seed = n)
  } else {
    l_ <- list(n, phash)
    # could try different algorithms.
    strhash <- digest(l_, algo='xxhash32')
    hash_ <- digest2int(strhash)
  }

  return(hash_)
}


# This is essentially set.seed(int, word)
seed_combiner_add_ints <- function(base_seed, hash_int) {
  # todo: this depends on the RNG... need to require a specific one, or hard-code limits. add tests accordingly.
  #
  # this works without error:
  # set.seed(2e9)
  # set.seed(-2e9)
  # so we require both ints to be less than 1e9 in absolute value.
  # the limit for each is 1/2 the limit of the 'seed' argument.
  # TODO: use .Machine$integer.max
  #       Actually no, that's a bad idea. Then the hash would depend on .Machine$integer.max.
  #       that value can change from machine to machine (and can be manually changed as well).
  #       Maybe check .Machine$integer.max only to give an *error* if too low.
  limit <- 1e9
  if (abs(hash_int) > limit) {
    # verbose output: message("hash_int was too large in absolute value, so was reduced via modulo.")
    hash_int <- hash_int %% limit
  }

  # might as well increase the range.
  # e.g., it might be that two different seeds give the same int but different signs
  # so this should make the probability of a hash conflict lower.
  sign_ <- sample(c(-1, 1), size = 1)
  # TODO: should we use the 'useHash' argument to sample.int ?
  int_from_base_seed <- sign_ * sample.int(n = limit, size = 1)

  seeding_int <- hash_int + int_from_base_seed
  return(seeding_int)
}


# todo: not thread-safe since writing to .GlobalEnv (?)
seed_hash_and_combine <- function(pnhasher, combiner, restore_seed = TRUE) {
  ret_fn <- function(base_seed, n, phash) {
    if (restore_seed) {
      entering_seed <- .Random.seed
    }
    assign(".Random.seed", base_seed, .GlobalEnv)

    hash_int <- pnhasher(n = n, phash = phash)
    seed_ <- combiner(base_seed = base_seed, hash_int = hash_int)

    if (restore_seed) {
      assign(".Random.seed", entering_seed, .GlobalEnv)
    }
    return(seed_)
  }
  return(ret_fn)
}


seed_init_add_ints <- seed_hash_and_combine(pnhasher = pnhasher_digest2int, combiner = seed_combiner_add_ints, restore_seed = TRUE)

seed_init_pnhash <- function(base_seed, n, phash) {
  # ignore base_seed
  rm(base_seed)

  seed_ <- pnhasher_digest2int(n = n, phash = phash)

  # return NULL and set here so we can temporarily impose our own RNG.
  # We impose our own RNG kind() for the same reason we impose our own
  # internal seed that does not depend on base seed.
  #
  # The choice of this particular kind is just because it is the default
  # in R 4.0. I leave open the possibility to change it in the future.
  # The important thing is that mc_reproduce() would still succeed.
  RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  set.seed(seed_)
  # We don't restore the RNGkind *here* because for this seed policy we want
  # to use the same RNG throughout.
  #
  # Alternative: We could force this RNG at a higher level, but it's nice to
  # limit it here so it doesn't affect other seed policies. We could condition
  # on 'seed_initializer' being 'seed_init_pnhash' but this way we avoid
  # conditioning.
  #
  # We do restore the user's RNGkind at a higher level (see on.exit() call in
  # do_mc_run()).
  return(NULL)
}
